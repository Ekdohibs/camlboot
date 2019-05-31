open Asttypes
open Parsetree

let trace = false
let tracearg_from = 742740000
let tracecur = ref 0

let debug =
  match Sys.getenv_opt "OCAMLINTERP_DEBUG" with
  | Some ("1" | "true" | "yes") -> true
  | Some ("0" | "false" | "no") -> false
  | Some other ->
    Printf.kprintf
      failwith
      "Error: unknown OCAMLINTERP_DEBUG value %S, use 'true' or 'false'"
      other
  | None ->
    (* default *)
    false

module SMap = Map.Make (String)
module SSet = Set.Make (String)

type value =
  | Int of int
  | Int64 of int64
  | Fun of arg_label * expression option * pattern * expression * env ref
  | Function of case list * env ref
  | String of bytes
  | Float of float
  | Tuple of value list
  | Constructor of string * int * value option
  | Prim of (value -> value)
  | ModVal of mdl
  | InChannel of in_channel
  | OutChannel of out_channel
  | Record of value ref SMap.t
  | SeqOr
  | SeqAnd
  | Lz of (unit -> value) ref
  | Array of value array
  | Fun_with_extra_args of value * value list * (arg_label * value) SMap.t

and env = (bool * value) SMap.t * (bool * mdl) SMap.t * (bool * int) SMap.t

and mdl =
  | Module of value SMap.t * mdl SMap.t * int SMap.t
  | Functor of string * module_expr * env

(* TODO: include arg restriction *)

exception InternalException of value

let rec pp_print_value ff = function
  | Int n -> Format.fprintf ff "%d" n
  | Int64 n -> Format.fprintf ff "%Ld" n
  | Fun _ | Function _ | Prim _ | SeqOr | SeqAnd | Lz _ | Fun_with_extra_args _
    ->
    Format.fprintf ff "<function>"
  | String s -> Format.fprintf ff "%S" (Bytes.to_string s)
  | Float f -> Format.fprintf ff "%f" f
  | Tuple l ->
    Format.fprintf
      ff
      "(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun ff () -> Format.fprintf ff ", ")
         pp_print_value)
      l
  | Constructor (c, d, None) -> Format.fprintf ff "%s#%d" c d
  | Constructor (c, d, Some v) ->
    Format.fprintf ff "%s#%d %a" c d pp_print_value v
  | ModVal _ -> Format.fprintf ff "<module>"
  | InChannel _ -> Format.fprintf ff "<in_channel>"
  | OutChannel _ -> Format.fprintf ff "<out_channel>"
  | Record r ->
    Format.fprintf ff "{";
    SMap.iter (fun k v -> Format.fprintf ff "%s = %a; " k pp_print_value !v) r;
    Format.fprintf ff "}"
  | Array a ->
    Format.fprintf
      ff
      "[|%a|]"
      (Format.pp_print_list
         ~pp_sep:(fun ff () -> Format.fprintf ff "; ")
         pp_print_value)
      (Array.to_list a)

let read_caml_int s =
  let c = ref 0L in
  let sign, init =
    if String.length s > 0 && s.[0] = '-' then (-1L, 1) else (1L, 0)
  in
  let base, init =
    if String.length s >= init + 2 && s.[init] = '0'
    then
      ( (match s.[init + 1] with
        | 'x' | 'X' -> 16L
        | 'b' | 'B' -> 2L
        | 'o' | 'O' -> 8L
        | _ -> assert false),
        init + 2 )
    else (10L, init)
  in
  for i = init to String.length s - 1 do
    match s.[i] with
    | '0' .. '9' as x ->
      c := Int64.(add (mul base !c) (of_int (int_of_char x - int_of_char '0')))
    | 'a' .. 'f' as x ->
      c :=
        Int64.(
          add (mul base !c) (of_int (int_of_char x - int_of_char 'a' + 10)))
    | 'A' .. 'F' as x ->
      c :=
        Int64.(
          add (mul base !c) (of_int (int_of_char x - int_of_char 'A' + 10)))
    | '_' -> ()
    | _ ->
      Format.eprintf "FIXME literal: %s@." s;
      assert false
  done;
  Int64.mul sign !c

let value_of_constant = function
  | Pconst_integer (s, (None | Some 'l')) ->
    Int (Int64.to_int (read_caml_int s))
  | Pconst_integer (s, Some ('L' | 'n')) -> Int64 (read_caml_int s)
  | Pconst_integer (s, Some c) ->
    Format.eprintf "Unsupported suffix %c@." c;
    assert false
  | Pconst_char c -> Int (int_of_char c)
  | Pconst_float (f, _) -> Float (float_of_string f)
  | Pconst_string (s, _) -> String (Bytes.of_string s)

let rec value_equal v1 v2 =
  match (v1, v2) with
  | Fun _, _
  | Function _, _
  | _, Fun _
  | _, Function _
  | SeqOr, _
  | SeqAnd, _
  | _, SeqOr
  | _, SeqAnd
  | Lz _, _
  | _, Lz _
  | Fun_with_extra_args _, _
  | _, Fun_with_extra_args _ ->
    failwith "tried to compare function"
  | ModVal _, _ | _, ModVal _ -> failwith "tried to compare module"
  | InChannel _, _ | OutChannel _, _ | _, InChannel _ | _, OutChannel _ ->
    failwith "tried to compare channel"
  | Int n1, Int n2 -> n1 = n2
  | Int64 n1, Int64 n2 -> n1 = n2
  | Float f1, Float f2 -> f1 = f2
  | String s1, String s2 -> s1 = s2
  | Constructor (c1, d1, None), Constructor (c2, d2, None) ->
    d1 = d2 && c1 = c2
  | Constructor (c1, d1, Some v1), Constructor (c2, d2, Some v2) ->
    d1 = d2 && c1 = c2 && value_equal v1 v2
  | Constructor _, Constructor _ -> false
  | Tuple l1, Tuple l2 ->
    assert (List.length l1 = List.length l2);
    List.for_all2 value_equal l1 l2
  | Record r1, Record r2 ->
    SMap.for_all
      (fun _ b -> b)
      (SMap.merge
         (fun _ u v ->
           match (u, v) with
           | None, None -> None
           | None, Some _ | Some _, None -> Some false
           | Some u, Some v -> Some (value_equal !u !v))
         r1
         r2)
  | Array a1, Array a2 ->
    if Array.length a1 <> Array.length a2
    then false
    else (
      let ok = ref true in
      for i = 0 to Array.length a1 - 1 do
        ok := !ok && value_equal a1.(i) a2.(i)
      done;
      !ok)
  | _ -> false

let rec value_compare v1 v2 =
  match (v1, v2) with
  | Fun _, _
  | Function _, _
  | _, Fun _
  | _, Function _
  | SeqOr, _
  | SeqAnd, _
  | _, SeqOr
  | _, SeqAnd
  | Lz _, _
  | _, Lz _
  | Fun_with_extra_args _, _
  | _, Fun_with_extra_args _ ->
    failwith "tried to compare function"
  | ModVal _, _ | _, ModVal _ -> failwith "tried to compare module"
  | InChannel _, _ | OutChannel _, _ | _, InChannel _ | _, OutChannel _ ->
    failwith "tried to compare channel"
  | Int n1, Int n2 -> compare n1 n2
  | Int64 n1, Int64 n2 -> compare n1 n2
  | Float f1, Float f2 -> compare f1 f2
  | String s1, String s2 -> compare s1 s2
  | Constructor (_, _, None), Constructor (_, _, Some _) -> -1
  | Constructor (_, _, Some _), Constructor (_, _, None) -> 1
  | Constructor (c1, d1, vv1), Constructor (c2, d2, vv2) ->
    let c = compare (d1, c1) (d2, c2) in
    if c <> 0
    then c
    else (
      match (vv1, vv2) with
      | None, None -> 0
      | Some v1, Some v2 -> value_compare v1 v2
      | _ -> assert false)
  | Tuple l1, Tuple l2 ->
    assert (List.length l1 = List.length l2);
    List.fold_left2
      (fun cur x y -> if cur = 0 then value_compare x y else cur)
      0
      l1
      l2
  | Record r1, Record r2 ->
    let map1 =
      SMap.merge
        (fun _ u v ->
          match (u, v) with
          | None, None -> None
          | None, Some _ | Some _, None -> assert false
          | Some u, Some v -> Some (!u, !v))
        r1
        r2
    in
    SMap.fold
      (fun _ (u, v) cur -> if cur = 0 then value_compare u v else cur)
      map1
      0
  | _ -> assert false

let value_lt v1 v2 = value_compare v1 v2 < 0
let value_le v1 v2 = value_compare v1 v2 <= 0
let value_gt v1 v2 = value_compare v1 v2 > 0
let value_ge v1 v2 = value_compare v1 v2 >= 0

exception Match_fail

let is_true = function
  | Constructor ("true", _, None) -> true
  | Constructor ("false", _, None) -> false
  | _ -> assert false

let rec lident_name = function
  | Longident.Lident s -> s
  | Longident.Ldot (_, s) -> s
  | Longident.Lapply (l1, l2) -> lident_name l2

let unit = Constructor ("()", 0, None)

let set_env env = function
  | Fun (_, _, _, _, ev) | Function (_, ev) -> ev := env
  | _ -> assert false

let rec eval_fun_or_function envref expr =
  match expr.pexp_desc with
  | Pexp_function cl -> Function (cl, envref)
  | Pexp_fun (label, default, p, e) -> Fun (label, default, p, e, envref)
  | Pexp_constraint (e, _) | Pexp_coerce (e, _, _) | Pexp_newtype (_, e) ->
    eval_fun_or_function envref e
  | _ -> failwith "unsupported rhs of rec"

let rec env_get_module ((_, module_env, _) as env) { txt = lident; loc } =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str module_env)
     with Not_found ->
       if debug
       then
         Format.eprintf
           "%a@.Module not found in env: %s@."
           Location.print_loc
           loc
           str;
       raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env { txt = ld; loc } in
    (match md with
    | Functor _ -> failwith "Ldot tried to access functor"
    | Module (_, md, _) ->
      (try SMap.find str md
       with Not_found ->
         if debug
         then
           Format.eprintf
             "%a@.Module not found in submodule: %s@."
             Location.print_loc
             loc
             (String.concat "." (Longident.flatten lident));
         raise Not_found))
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_value ((value_env, _, _) as env) { txt = lident; loc } =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str value_env)
     with Not_found ->
       if debug
       then
         Format.eprintf
           "%a@.Variable not found in env: %s@."
           Location.print_loc
           loc
           str;
       raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env { txt = ld; loc } in
    (match md with
    | Functor _ -> failwith "Ldot tried to access functor"
    | Module (md, _, _) ->
      (try SMap.find str md
       with Not_found ->
         if debug
         then
           Format.eprintf
             "%a@.Value not found in submodule: %s@."
             Location.print_loc
             loc
             (String.concat "." (Longident.flatten lident));
         raise Not_found))
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_constr ((_, _, constr_env) as env) { txt = lident; loc } =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str constr_env)
     with Not_found ->
       if debug
       then
         Format.eprintf
           "%a@.Constructor not found in env: %s@."
           Location.print_loc
           loc
           str;
       raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env { txt = ld; loc } in
    (match md with
    | Functor _ -> failwith "Ldot tried to access functor"
    | Module (_, _, md) ->
      (try SMap.find str md
       with Not_found ->
         if debug
         then
           Format.eprintf
             "%a@.Constructor not found in submodule: %s@."
             Location.print_loc
             loc
             (String.concat "." (Longident.flatten lident));
         raise Not_found))
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_set_value key v (ve, me, ce) = (SMap.add key (true, v) ve, me, ce)
let env_set_module key m (ve, me, ce) = (ve, SMap.add key (true, m) me, ce)
let env_set_constr key c (ve, me, ce) = (ve, me, SMap.add key (true, c) ce)

let env_extend exported (ve, me, ce) (ve1, me1, ce1) =
  let nve = SMap.fold (fun key v ve -> SMap.add key (exported, v) ve) ve1 ve in
  let nme = SMap.fold (fun key m me -> SMap.add key (exported, m) me) me1 me in
  let nce = SMap.fold (fun key c ce -> SMap.add key (exported, c) ce) ce1 ce in
  (nve, nme, nce)

let make_module (ve, me, ce) =
  let ve = SMap.map snd (SMap.filter (fun _ (b, _) -> b) ve) in
  let me = SMap.map snd (SMap.filter (fun _ (b, _) -> b) me) in
  let ce = SMap.map snd (SMap.filter (fun _ (b, _) -> b) ce) in
  Module (ve, me, ce)

let prevent_export (ve, me, ce) =
  let ve = SMap.map (fun (_, x) -> (false, x)) ve in
  let me = SMap.map (fun (_, x) -> (false, x)) me in
  let ce = SMap.map (fun (_, x) -> (false, x)) ce in
  (ve, me, ce)

let empty_env = (SMap.empty, SMap.empty, SMap.empty)

(* HACK *)
let cur_env = ref empty_env

let rec seeded_hash_param meaningful total seed = function
  | Int n -> Hashtbl.seeded_hash seed n
  | Int64 n -> Hashtbl.seeded_hash seed n
  | Float f -> Hashtbl.seeded_hash seed f
  | Tuple l -> 0
  | String s -> Hashtbl.seeded_hash seed (Bytes.to_string s)
  | Constructor (c, _, v) -> Hashtbl.seeded_hash seed c
  | Array a -> 0
  | Record r -> 0
  | Fun _ | Function _ | SeqOr | SeqAnd | InChannel _ | OutChannel _ | Prim _
  | Lz _ | ModVal _ | Fun_with_extra_args _ ->
    assert false

let prim1 f unwrap1 wrap = Prim (fun x -> wrap (f (unwrap1 x)))

let prim2 f unwrap1 unwrap2 wrap =
  Prim (fun x -> prim1 (f (unwrap1 x)) unwrap2 wrap)

let prim3 f unwrap1 unwrap2 unwrap3 wrap =
  Prim (fun x -> prim2 (f (unwrap1 x)) unwrap2 unwrap3 wrap)

let prim4 f unwrap1 unwrap2 unwrap3 unwrap4 wrap =
  Prim (fun x -> prim3 (f (unwrap1 x)) unwrap2 unwrap3 unwrap4 wrap)

let prim5 f unwrap1 unwrap2 unwrap3 unwrap4 unwrap5 wrap =
  Prim (fun x -> prim4 (f (unwrap1 x)) unwrap2 unwrap3 unwrap4 unwrap5 wrap)

let wrap_int n = Int n

let unwrap_int v =
  match v with
  | Int n -> n
  | _ -> assert false

let wrap_int64 n = Int64 n

let unwrap_int64 v =
  match v with
  | Int64 n -> n
  | _ -> assert false

let wrap_float f = Float f

let unwrap_float v =
  match v with
  | Float f -> f
  | _ -> assert false

let unwrap_bool = is_true

let wrap_bool b =
  if b then Constructor ("true", 1, None) else Constructor ("false", 0, None)

let wrap_unit () = unit

let unwrap_unit = function
  | Constructor ("()", _, None) -> ()
  | _ -> assert false

let wrap_bytes s = String s

let unwrap_bytes = function
  | String s -> s
  | _ -> assert false

let wrap_string s = String (Bytes.of_string s)

let unwrap_string = function
  | String s -> Bytes.to_string s
  | _ -> assert false

let wrap_string_unsafe s = String (Bytes.unsafe_of_string s)

let unwrap_string_unsafe = function
  | String s -> Bytes.unsafe_to_string s
  | _ -> assert false

let wrap_char c = Int (int_of_char c)

let unwrap_char = function
  | Int n -> char_of_int (n land 255)
  | _ -> assert false

let wrap_array wrapf a = Array (Array.map wrapf a)

let unwrap_array unwrapf = function
  | Array a -> Array.map unwrapf a
  | _ -> assert false

let wrap_array_id a = Array a

let unwrap_array_id = function
  | Array a -> a
  | _ -> assert false

let wrap_in_channel ic = InChannel ic

let unwrap_in_channel = function
  | InChannel ic -> ic
  | _ -> assert false

let wrap_out_channel oc = OutChannel oc

let unwrap_out_channel = function
  | OutChannel oc -> oc
  | _ -> assert false

let cc x d = Constructor (x, d, None)

let wrap_open_flag = function
  | Open_rdonly -> cc "Open_rdonly" 0
  | Open_wronly -> cc "Open_wronly" 1
  | Open_append -> cc "Open_append" 2
  | Open_creat -> cc "Open_creat" 3
  | Open_trunc -> cc "Open_trunc" 4
  | Open_excl -> cc "Open_excl" 5
  | Open_binary -> cc "Open_binary" 6
  | Open_text -> cc "Open_text" 7
  | Open_nonblock -> cc "Open_nonblock" 8

let unwrap_open_flag = function
  | Constructor ("Open_rdonly", _, None) -> Open_rdonly
  | Constructor ("Open_wronly", _, None) -> Open_wronly
  | Constructor ("Open_append", _, None) -> Open_append
  | Constructor ("Open_creat", _, None) -> Open_creat
  | Constructor ("Open_trunc", _, None) -> Open_trunc
  | Constructor ("Open_excl", _, None) -> Open_excl
  | Constructor ("Open_binary", _, None) -> Open_binary
  | Constructor ("Open_text", _, None) -> Open_text
  | Constructor ("Open_nonblock", _, None) -> Open_nonblock
  | _ -> assert false

let rec wrap_list wrapf = function
  | [] -> cc "[]" 0
  | x :: l -> Constructor ("::", 0, Some (Tuple [ wrapf x; wrap_list wrapf l ]))

let rec unwrap_list unwrapf = function
  | Constructor ("[]", _, None) -> []
  | Constructor ("::", _, Some (Tuple [ x; l ])) ->
    unwrapf x :: unwrap_list unwrapf l
  | _ -> assert false

let unwrap_marshal_flag = function
  | Constructor ("No_sharing", _, None) -> Marshal.No_sharing
  | Constructor ("Closures", _, None) -> Marshal.Closures
  | Constructor ("Compat_32", _, None) -> Marshal.Compat_32
  | _ -> assert false

external open_descriptor_out
  :  int ->
  out_channel
  = "caml_ml_open_descriptor_out"

external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"
external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"

external set_out_channel_name
  :  out_channel ->
  string ->
  unit
  = "caml_ml_set_channel_name"

external out_channels_list
  :  unit ->
  out_channel list
  = "caml_ml_out_channels_list"

external unsafe_output
  :  out_channel ->
  bytes ->
  int ->
  int ->
  unit
  = "caml_ml_output_bytes"

external unsafe_output_string
  :  out_channel ->
  string ->
  int ->
  int ->
  unit
  = "caml_ml_output"

external set_in_channel_name
  :  in_channel ->
  string ->
  unit
  = "caml_ml_set_channel_name"

external unsafe_input
  :  in_channel ->
  bytes ->
  int ->
  int ->
  int
  = "caml_ml_input"

external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"
external random_seed : unit -> int array = "caml_sys_random_seed"

external digest_unsafe_string
  :  string ->
  int ->
  int ->
  string
  = "caml_md5_string"

external marshal_to_channel
  :  out_channel ->
  'a ->
  unit list ->
  unit
  = "caml_output_value"

external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external input_scan_line : in_channel -> int = "caml_ml_input_scan_line"

let unwrap_position = function
  | Record r ->
    Lexing.
      { pos_fname = unwrap_string !(SMap.find "pos_fname" r);
        pos_lnum = unwrap_int !(SMap.find "pos_lnum" r);
        pos_bol = unwrap_int !(SMap.find "pos_bol" r);
        pos_cnum = unwrap_int !(SMap.find "pos_cnum" r)
      }
  | _ -> assert false

let wrap_position Lexing.{ pos_fname; pos_lnum; pos_bol; pos_cnum } =
  Record
    (SMap.of_seq
    @@ List.to_seq
         [ ("pos_fname", ref (wrap_string pos_fname));
           ("pos_lnum", ref (wrap_int pos_lnum));
           ("pos_bol", ref (wrap_int pos_bol));
           ("pos_cnum", ref (wrap_int pos_cnum))
         ])

let wrap_gc_stat
    Gc.
      { minor_words;
        promoted_words;
        major_words;
        minor_collections;
        major_collections;
        heap_words;
        heap_chunks;
        live_words;
        live_blocks;
        free_words;
        free_blocks;
        largest_free;
        fragments;
        compactions;
        top_heap_words;
        stack_size
      }
  =
  Record
    (SMap.of_seq
    @@ List.to_seq
         [ ("minor_words", ref (wrap_float minor_words));
           ("promoted_words", ref (wrap_float promoted_words));
           ("major_words", ref (wrap_float major_words));
           ("minor_collections", ref (wrap_int minor_collections));
           ("major_collections", ref (wrap_int major_collections));
           ("heap_words", ref (wrap_int heap_words));
           ("heap_chunks", ref (wrap_int heap_chunks));
           ("live_words", ref (wrap_int live_words));
           ("live_blocks", ref (wrap_int live_blocks));
           ("free_words", ref (wrap_int free_words));
           ("free_blocks", ref (wrap_int free_blocks));
           ("largest_free", ref (wrap_int largest_free));
           ("fragments", ref (wrap_int fragments));
           ("compactions", ref (wrap_int compactions));
           ("top_heap_words", ref (wrap_int top_heap_words));
           ("stack_size", ref (wrap_int stack_size))
         ])

type parser_env =
  { mutable s_stack : int array;
    (* States *)
    mutable v_stack : Obj.t array;
    (* Semantic attributes *)
    mutable symb_start_stack : Lexing.position array;
    (* Start positions *)
    mutable symb_end_stack : Lexing.position array;
    (* End positions *)
    mutable stacksize : int;
    (* Size of the stacks *)
    mutable stackbase : int;
    (* Base sp for current parse *)
    mutable curr_char : int;
    (* Last token read *)
    mutable lval : Obj.t;
    (* Its semantic attribute *)
    mutable symb_start : Lexing.position;
    (* Start pos. of the current symbol*)
    mutable symb_end : Lexing.position;
    (* End pos. of the current symbol *)
    mutable asp : int;
    (* The stack pointer for attributes *)
    mutable rule_len : int;
    (* Number of rhs items in the rule *)
    mutable rule_number : int;
    (* Rule number to reduce by *)
    mutable sp : int;
    (* Saved sp for parse_engine *)
    mutable state : int;
    (* Saved state for parse_engine *)
    mutable errflag : int
    }

(* Saved error flag for parse_engine *)

type parse_tables =
  { actions : (parser_env -> Obj.t) array;
    transl_const : int array;
    transl_block : int array;
    lhs : string;
    len : string;
    defred : string;
    dgoto : string;
    sindex : string;
    rindex : string;
    gindex : string;
    tablesize : int;
    table : string;
    check : string;
    error_function : string -> unit;
    names_const : string;
    names_block : string
    }

type parser_input =
  | Start
  | Token_read
  | Stacks_grown_1
  | Stacks_grown_2
  | Semantic_action_computed
  | Error_detected

let unwrap_parser_input = function
  | Constructor ("Start", _, None) -> Start
  | Constructor ("Token_read", _, None) -> Token_read
  | Constructor ("Stacks_grown_1", _, None) -> Stacks_grown_1
  | Constructor ("Stacks_grown_2", _, None) -> Stacks_grown_2
  | Constructor ("Semantic_action_computed", _, None) ->
    Semantic_action_computed
  | Constructor ("Error_detected", _, None) -> Error_detected
  | _ -> assert false

type parser_output =
  | Read_token
  | Raise_parse_error
  | Grow_stacks_1
  | Grow_stacks_2
  | Compute_semantic_action
  | Call_error_function

let wrap_parser_output = function
  | Read_token -> cc "Read_token" 0
  | Raise_parse_error -> cc "Raise_parse_error" 1
  | Grow_stacks_1 -> cc "Grow_stacks_1" 2
  | Grow_stacks_2 -> cc "Grow_stacks_2" 3
  | Compute_semantic_action -> cc "Compute_semantic_action" 4
  | Call_error_function -> cc "Call_error_function" 5

let apply_ref = ref (fun _ _ -> assert false)

let unwrap_parser_env = function
  | Record r ->
    { s_stack = unwrap_array unwrap_int !(SMap.find "s_stack" r);
      v_stack = Obj.magic (unwrap_array_id !(SMap.find "v_stack" r));
      symb_start_stack =
        unwrap_array unwrap_position !(SMap.find "symb_start_stack" r);
      symb_end_stack =
        unwrap_array unwrap_position !(SMap.find "symb_end_stack" r);
      stacksize = unwrap_int !(SMap.find "stacksize" r);
      stackbase = unwrap_int !(SMap.find "stackbase" r);
      curr_char = unwrap_int !(SMap.find "curr_char" r);
      lval = Obj.repr !(SMap.find "lval" r);
      symb_start = unwrap_position !(SMap.find "symb_start" r);
      symb_end = unwrap_position !(SMap.find "symb_end" r);
      asp = unwrap_int !(SMap.find "asp" r);
      rule_len = unwrap_int !(SMap.find "rule_len" r);
      rule_number = unwrap_int !(SMap.find "rule_number" r);
      sp = unwrap_int !(SMap.find "sp" r);
      state = unwrap_int !(SMap.find "state" r);
      errflag = unwrap_int !(SMap.find "errflag" r)
    }
  | _ -> assert false

let sync_parser_env pe = function
  | Record r ->
    let open Parsing in
    SMap.find "s_stack" r := wrap_array wrap_int pe.s_stack;
    SMap.find "v_stack" r := wrap_array_id (Obj.magic pe.v_stack);
    SMap.find "symb_start_stack" r
    := wrap_array wrap_position pe.symb_start_stack;
    SMap.find "symb_end_stack" r := wrap_array wrap_position pe.symb_end_stack;
    SMap.find "stacksize" r := wrap_int pe.stacksize;
    SMap.find "stackbase" r := wrap_int pe.stackbase;
    SMap.find "curr_char" r := wrap_int pe.curr_char;
    SMap.find "lval" r := Obj.obj pe.lval;
    SMap.find "symb_start" r := wrap_position pe.symb_start;
    SMap.find "symb_end" r := wrap_position pe.symb_end;
    SMap.find "asp" r := wrap_int pe.asp;
    SMap.find "rule_len" r := wrap_int pe.rule_len;
    SMap.find "rule_number" r := wrap_int pe.rule_number;
    SMap.find "sp" r := wrap_int pe.sp;
    SMap.find "state" r := wrap_int pe.state;
    SMap.find "errflag" r := wrap_int pe.errflag
  | _ -> assert false

let unwrap_parse_tables syncenv = function
  | Record r ->
    let actions =
      unwrap_array
        (fun fv pe ->
          sync_parser_env pe syncenv;
          Obj.repr (!apply_ref fv [ (Nolabel, syncenv) ]))
        !(SMap.find "actions" r)
    in
    { actions;
      transl_const = unwrap_array unwrap_int !(SMap.find "transl_const" r);
      transl_block = unwrap_array unwrap_int !(SMap.find "transl_block" r);
      lhs = unwrap_string_unsafe !(SMap.find "lhs" r);
      len = unwrap_string_unsafe !(SMap.find "len" r);
      defred = unwrap_string_unsafe !(SMap.find "defred" r);
      dgoto = unwrap_string_unsafe !(SMap.find "dgoto" r);
      sindex = unwrap_string_unsafe !(SMap.find "sindex" r);
      rindex = unwrap_string_unsafe !(SMap.find "rindex" r);
      gindex = unwrap_string_unsafe !(SMap.find "gindex" r);
      tablesize = unwrap_int !(SMap.find "tablesize" r);
      table = unwrap_string_unsafe !(SMap.find "table" r);
      check = unwrap_string_unsafe !(SMap.find "check" r);
      error_function =
        (fun s ->
          unwrap_unit
            (!apply_ref
               !(SMap.find "error_function" r)
               [ (Nolabel, wrap_string s) ]));
      names_const = unwrap_string_unsafe !(SMap.find "names_const" r);
      names_block = unwrap_string_unsafe !(SMap.find "names_block" r)
    }
  | _ -> assert false

external parse_engine
  :  parse_tables ->
  parser_env ->
  parser_input ->
  Obj.t ->
  parser_output
  = "caml_parse_engine"

external lex_engine
  :  Lexing.lex_tables ->
  int ->
  Lexing.lexbuf ->
  int
  = "caml_lex_engine"

external new_lex_engine
  :  Lexing.lex_tables ->
  int ->
  Lexing.lexbuf ->
  int
  = "caml_new_lex_engine"

let parse_engine_wrapper tables env input token =
  let nenv = unwrap_parser_env env in
  let tbls = unwrap_parse_tables env tables in
  let obj =
    if input = Semantic_action_computed
    then Obj.repr token
    else (
      match token with
      | Constructor (c, d, None) -> Obj.repr d
      | Constructor (c, d, Some arg) ->
        let w = Obj.repr (Some arg) in
        Obj.set_tag w d;
        w
      | _ -> assert false)
  in
  let res = parse_engine tbls nenv input obj in
  sync_parser_env nenv env;
  res

let unwrap_lexbuf v =
  match v with
  | Record r ->
    let open Lexing in
    { refill_buff = (fun _ -> assert false);
      lex_buffer = unwrap_bytes !(SMap.find "lex_buffer" r);
      lex_buffer_len = unwrap_int !(SMap.find "lex_buffer_len" r);
      lex_abs_pos = unwrap_int !(SMap.find "lex_abs_pos" r);
      lex_start_pos = unwrap_int !(SMap.find "lex_start_pos" r);
      lex_curr_pos = unwrap_int !(SMap.find "lex_curr_pos" r);
      lex_last_pos = unwrap_int !(SMap.find "lex_last_pos" r);
      lex_last_action = unwrap_int !(SMap.find "lex_last_action" r);
      lex_eof_reached = unwrap_bool !(SMap.find "lex_eof_reached" r);
      lex_mem = unwrap_array unwrap_int !(SMap.find "lex_mem" r);
      lex_start_p = unwrap_position !(SMap.find "lex_start_p" r);
      lex_curr_p = unwrap_position !(SMap.find "lex_curr_p" r)
    }
  | _ -> assert false

let sync_lexbuf v lb =
  match v with
  | Record r ->
    let open Lexing in
    SMap.find "lex_buffer" r := wrap_bytes lb.lex_buffer;
    SMap.find "lex_buffer_len" r := wrap_int lb.lex_buffer_len;
    SMap.find "lex_abs_pos" r := wrap_int lb.lex_abs_pos;
    SMap.find "lex_start_pos" r := wrap_int lb.lex_start_pos;
    SMap.find "lex_curr_pos" r := wrap_int lb.lex_curr_pos;
    SMap.find "lex_last_pos" r := wrap_int lb.lex_last_pos;
    SMap.find "lex_last_action" r := wrap_int lb.lex_last_action;
    SMap.find "lex_eof_reached" r := wrap_bool lb.lex_eof_reached;
    SMap.find "lex_mem" r := wrap_array wrap_int lb.lex_mem;
    SMap.find "lex_start_p" r := wrap_position lb.lex_start_p;
    SMap.find "lex_curr_p" r := wrap_position lb.lex_curr_p
  | _ -> assert false

let unwrap_lex_tables = function
  | Record r ->
    let gs f = unwrap_string_unsafe !(SMap.find f r) in
    let open Lexing in
    { lex_base = gs "lex_base";
      lex_backtrk = gs "lex_backtrk";
      lex_default = gs "lex_default";
      lex_trans = gs "lex_trans";
      lex_check = gs "lex_check";
      lex_base_code = gs "lex_base_code";
      lex_backtrk_code = gs "lex_backtrk_code";
      lex_default_code = gs "lex_default_code";
      lex_trans_code = gs "lex_trans_code";
      lex_check_code = gs "lex_check_code";
      lex_code = gs "lex_code"
    }
  | _ -> assert false

let lex_engine_wrapper tables n lexbuf =
  let nbuf = unwrap_lexbuf lexbuf in
  let tbls = unwrap_lex_tables tables in
  let res = lex_engine tbls n nbuf in
  sync_lexbuf lexbuf nbuf;
  res

let new_lex_engine_wrapper tables n lexbuf =
  let nbuf = unwrap_lexbuf lexbuf in
  let tbls = unwrap_lex_tables tables in
  let res = new_lex_engine tbls n nbuf in
  sync_lexbuf lexbuf nbuf;
  res

let id x = x

let parse_engine_prim =
  prim4 parse_engine_wrapper id id unwrap_parser_input id wrap_parser_output

let lex_engine_prim = prim3 lex_engine_wrapper id unwrap_int id wrap_int

let new_lex_engine_prim =
  prim3 new_lex_engine_wrapper id unwrap_int id wrap_int

let initial_env = ref (empty_env : env)
let exn_id = ref 0

let declare_builtin_constructor name d =
  initial_env := env_set_constr name d !initial_env

let declare_exn name =
  let d = !exn_id in
  incr exn_id;
  declare_builtin_constructor name d;
  d

let not_found_exn_id = declare_exn "Not_found"
let not_found_exn = Constructor ("Not_found", not_found_exn_id, None)
let _ = declare_exn "Exit"
let _ = declare_exn "Invalid_argument"
let _ = declare_exn "Failure"
let _ = declare_exn "Match_failure"
let assert_failure_id = declare_exn "Assert_failure"
let _ = declare_exn "Sys_blocked_io"
let _ = declare_exn "Sys_error"
let _ = declare_exn "End_of_file"
let _ = declare_exn "Division_by_zero"
let _ = declare_exn "Undefined_recursive_module"
let _ = declare_builtin_constructor "false" 0
let _ = declare_builtin_constructor "true" 1
let _ = declare_builtin_constructor "None" 0
let _ = declare_builtin_constructor "Some" 0
let _ = declare_builtin_constructor "[]" 0
let _ = declare_builtin_constructor "::" 0
let _ = declare_builtin_constructor "()" 0

let prims =
  [ ("%apply", Prim (fun vf -> Prim (fun v -> !apply_ref vf [ (Nolabel, v) ])));
    ( "%revapply",
      Prim (fun v -> Prim (fun vf -> !apply_ref vf [ (Nolabel, v) ])) );
    ("%raise", Prim (fun v -> raise (InternalException v)));
    ("%reraise", Prim (fun v -> raise (InternalException v)));
    ("%raise_notrace", Prim (fun v -> raise (InternalException v)));
    ("%sequand", SeqAnd);
    ("%sequor", SeqOr);
    ("%boolnot", prim1 not unwrap_bool wrap_bool);
    ("%negint", prim1 ( ~- ) unwrap_int wrap_int);
    ("%succint", prim1 succ unwrap_int wrap_int);
    ("%predint", prim1 pred unwrap_int wrap_int);
    ("%addint", prim2 ( + ) unwrap_int unwrap_int wrap_int);
    ("%subint", prim2 ( - ) unwrap_int unwrap_int wrap_int);
    ("%mulint", prim2 ( * ) unwrap_int unwrap_int wrap_int);
    ("%divint", prim2 ( / ) unwrap_int unwrap_int wrap_int);
    ("%modint", prim2 ( mod ) unwrap_int unwrap_int wrap_int);
    ("%andint", prim2 ( land ) unwrap_int unwrap_int wrap_int);
    ("%orint", prim2 ( lor ) unwrap_int unwrap_int wrap_int);
    ("%xorint", prim2 ( lxor ) unwrap_int unwrap_int wrap_int);
    ("%lslint", prim2 ( lsl ) unwrap_int unwrap_int wrap_int);
    ("%lsrint", prim2 ( lsr ) unwrap_int unwrap_int wrap_int);
    ("%asrint", prim2 ( asr ) unwrap_int unwrap_int wrap_int);
    ("%addfloat", prim2 ( +. ) unwrap_float unwrap_float wrap_float);
    ("%subfloat", prim2 ( -. ) unwrap_float unwrap_float wrap_float);
    ("%mulfloat", prim2 ( *. ) unwrap_float unwrap_float wrap_float);
    ("%divfloat", prim2 ( /. ) unwrap_float unwrap_float wrap_float);
    ("%floatofint", prim1 float_of_int unwrap_int wrap_float);
    ("%intoffloat", prim1 int_of_float unwrap_float wrap_int);
    ("%lessthan", prim2 value_lt id id wrap_bool);
    ("%lessequal", prim2 value_le id id wrap_bool);
    ("%greaterthan", prim2 value_gt id id wrap_bool);
    ("%greaterequal", prim2 value_ge id id wrap_bool);
    ("%compare", prim2 value_compare id id wrap_int);
    ("%equal", prim2 value_equal id id wrap_bool);
    ("%notequal", prim2 value_equal id id (fun x -> wrap_bool (not x)));
    ("%eq", prim2 ( == ) id id wrap_bool);
    ("%noteq", prim2 ( != ) id id wrap_bool);
    ("%identity", Prim (fun x -> x));
    ("caml_register_named_value", Prim (fun _ -> Prim (fun _ -> unit)));
    ( "caml_int64_float_of_bits",
      prim1 Int64.float_of_bits unwrap_int64 wrap_float );
    ( "caml_ml_open_descriptor_out",
      prim1 open_descriptor_out unwrap_int wrap_out_channel );
    ( "caml_ml_open_descriptor_in",
      prim1 open_descriptor_in unwrap_int wrap_in_channel );
    ( "caml_sys_open",
      prim3
        open_desc
        unwrap_string
        (unwrap_list unwrap_open_flag)
        unwrap_int
        wrap_int );
    ( "caml_ml_set_channel_name",
      prim2
        (fun v s ->
          match v with
          | InChannel ic -> set_in_channel_name ic s
          | OutChannel oc -> set_out_channel_name oc s
          | _ -> assert false)
        id
        unwrap_string
        wrap_unit );
    ( "caml_ml_close_channel",
      prim1
        (function
          | InChannel ic -> close_in ic
          | OutChannel oc -> close_out oc
          | _ -> assert false)
        id
        wrap_unit );
    ( "caml_ml_out_channels_list",
      prim1 out_channels_list unwrap_unit (wrap_list wrap_out_channel) );
    ( "caml_ml_output_bytes",
      prim4
        unsafe_output
        unwrap_out_channel
        unwrap_bytes
        unwrap_int
        unwrap_int
        wrap_unit );
    ( "caml_ml_output",
      prim4
        unsafe_output_string
        unwrap_out_channel
        unwrap_string
        unwrap_int
        unwrap_int
        wrap_unit );
    ( "caml_ml_output_int",
      prim2 output_binary_int unwrap_out_channel unwrap_int wrap_unit );
    ( "caml_ml_output_char",
      prim2 output_char unwrap_out_channel unwrap_char wrap_unit );
    ("caml_ml_flush", prim1 flush unwrap_out_channel wrap_unit);
    ("caml_ml_input_char", prim1 input_char unwrap_in_channel wrap_char);
    ("caml_ml_input_int", prim1 input_binary_int unwrap_in_channel wrap_int);
    ( "caml_ml_input_scan_line",
      prim1 input_scan_line unwrap_in_channel wrap_int );
    ( "caml_ml_input",
      prim4
        unsafe_input
        unwrap_in_channel
        unwrap_bytes
        unwrap_int
        unwrap_int
        wrap_int );
    ("caml_ml_seek_in", prim2 seek_in unwrap_in_channel unwrap_int wrap_unit);
    ("caml_ml_pos_out", prim1 pos_out unwrap_out_channel wrap_int);
    ("caml_ml_pos_in", prim1 pos_in unwrap_in_channel wrap_int);
    ("caml_ml_seek_out", prim2 seek_out unwrap_out_channel unwrap_int wrap_unit);
    ("%makemutable", Prim (fun v -> Record (SMap.singleton "contents" (ref v))));
    ( "%field0",
      Prim
        (function
        | Record r -> !(SMap.find "contents" r)
        | Tuple l -> List.hd l
        | _ -> assert false) );
    ( "%field1",
      Prim
        (function
        | Tuple l -> List.hd (List.tl l)
        | _ -> assert false) );
    ( "%setfield0",
      Prim
        (function
        | Record r ->
          Prim
            (fun v ->
              SMap.find "contents" r := v;
              unit)
        | _ -> assert false) );
    ( "%incr",
      Prim
        (function
        | Record r ->
          let z = SMap.find "contents" r in
          z := wrap_int (unwrap_int !z + 1);
          unit
        | _ -> assert false) );
    ( "%decr",
      Prim
        (function
        | Record r ->
          let z = SMap.find "contents" r in
          z := wrap_int (unwrap_int !z - 1);
          unit
        | _ -> assert false) );
    ("%ignore", Prim (fun _ -> unit));
    ("caml_format_int", prim2 format_int unwrap_string unwrap_int wrap_string);
    ( "caml_format_float",
      prim2 format_float unwrap_string unwrap_float wrap_string );
    ("caml_int_of_string", prim1 int_of_string unwrap_string wrap_int);
    ( "caml_output_value",
      prim3
        marshal_to_channel
        unwrap_out_channel
        id
        (unwrap_list unwrap_unit)
        wrap_unit );
    ( "caml_output_value_to_buffer",
      prim5
        Marshal.to_buffer
        unwrap_bytes
        unwrap_int
        unwrap_int
        id
        (unwrap_list unwrap_marshal_flag)
        wrap_int );
    ("caml_input_value", prim1 input_value unwrap_in_channel id);
    ("caml_sys_exit", prim1 exit unwrap_int wrap_unit);
    ("caml_parse_engine", parse_engine_prim);
    ("caml_lex_engine", lex_engine_prim);
    ("caml_new_lex_engine", new_lex_engine_prim);
    (* Sys *)
    ( "caml_sys_get_argv",
      Prim
        (fun _ ->
          Tuple [ wrap_string ""; Array (Array.map wrap_string Sys.argv) ]) );
    ( "caml_sys_get_config",
      Prim (fun _ -> Tuple [ wrap_string "Unix"; Int 0; wrap_bool true ]) );
    ("%big_endian", Prim (fun _ -> wrap_bool Sys.big_endian));
    ("%word_size", Prim (fun _ -> Int 64));
    ("%int_size", Prim (fun _ -> Int 64));
    ("%max_wosize", Prim (fun _ -> Int 1000000));
    ("%ostype_unix", Prim (fun _ -> wrap_bool false));
    ("%ostype_win32", Prim (fun _ -> wrap_bool false));
    ("%ostype_cygwin", Prim (fun _ -> wrap_bool false));
    ( "%backend_type",
      Prim (fun _ -> Constructor ("Other", 0, Some (wrap_string "Interpreter")))
    );
    ("caml_sys_getenv", Prim (fun _ -> raise (InternalException not_found_exn)));
    ("caml_sys_file_exists", prim1 Sys.file_exists unwrap_string wrap_bool);
    ("caml_sys_getcwd", prim1 Sys.getcwd unwrap_unit wrap_string);
    ("caml_sys_rename", prim2 Sys.rename unwrap_string unwrap_string wrap_unit);
    ("caml_sys_remove", prim1 Sys.remove unwrap_string wrap_unit);
    (* Bytes *)
    ("caml_create_bytes", prim1 Bytes.create unwrap_int wrap_bytes);
    ( "caml_fill_bytes",
      prim4
        Bytes.unsafe_fill
        unwrap_bytes
        unwrap_int
        unwrap_int
        unwrap_char
        wrap_unit );
    ("%bytes_to_string", Prim (fun v -> v));
    ("%bytes_of_string", Prim (fun v -> v));
    ("%string_length", prim1 Bytes.length unwrap_bytes wrap_int);
    ("%bytes_length", prim1 Bytes.length unwrap_bytes wrap_int);
    ("%string_safe_get", prim2 Bytes.get unwrap_bytes unwrap_int wrap_char);
    ( "%string_unsafe_get",
      prim2 Bytes.unsafe_get unwrap_bytes unwrap_int wrap_char );
    ("%bytes_safe_get", prim2 Bytes.get unwrap_bytes unwrap_int wrap_char);
    ( "%bytes_unsafe_get",
      prim2 Bytes.unsafe_get unwrap_bytes unwrap_int wrap_char );
    ( "%bytes_safe_set",
      prim3 Bytes.set unwrap_bytes unwrap_int unwrap_char wrap_unit );
    ( "%bytes_unsafe_set",
      prim3 Bytes.unsafe_set unwrap_bytes unwrap_int unwrap_char wrap_unit );
    ( "caml_blit_string",
      prim5
        String.blit
        unwrap_string
        unwrap_int
        unwrap_bytes
        unwrap_int
        unwrap_int
        wrap_unit );
    ( "caml_blit_bytes",
      prim5
        Bytes.blit
        unwrap_bytes
        unwrap_int
        unwrap_bytes
        unwrap_int
        unwrap_int
        wrap_unit );
    (* Lazy *)
    ( "%lazy_force",
      Prim
        (function
        | Lz f ->
          let v = !f () in
          (f := fun () -> v);
          v
        | _ -> assert false) );
    (* Int64 *)
    ("%int64_neg", prim1 Int64.neg unwrap_int64 wrap_int64);
    ("%int64_add", prim2 Int64.add unwrap_int64 unwrap_int64 wrap_int64);
    ("%int64_sub", prim2 Int64.sub unwrap_int64 unwrap_int64 wrap_int64);
    ("%int64_mul", prim2 Int64.mul unwrap_int64 unwrap_int64 wrap_int64);
    ("%int64_div", prim2 Int64.div unwrap_int64 unwrap_int64 wrap_int64);
    ("%int64_mod", prim2 Int64.rem unwrap_int64 unwrap_int64 wrap_int64);
    ("%int64_and", prim2 Int64.logand unwrap_int64 unwrap_int64 wrap_int64);
    ("%int64_or", prim2 Int64.logor unwrap_int64 unwrap_int64 wrap_int64);
    ("%int64_xor", prim2 Int64.logxor unwrap_int64 unwrap_int64 wrap_int64);
    ("%int64_lsl", prim2 Int64.shift_left unwrap_int64 unwrap_int wrap_int64);
    ( "%int64_lsr",
      prim2 Int64.shift_right_logical unwrap_int64 unwrap_int wrap_int64 );
    ("%int64_asr", prim2 Int64.shift_right unwrap_int64 unwrap_int wrap_int64);
    ("%int64_of_int", prim1 Int64.of_int unwrap_int wrap_int64);
    ("%int64_to_int", prim1 Int64.to_int unwrap_int64 wrap_int);
    ("caml_int64_of_string", prim1 Int64.of_string unwrap_string wrap_int64);
    (* Int32 *)
    ("caml_int32_of_string", prim1 int_of_string unwrap_string wrap_int);
    ("%int32_neg", prim1 ( ~- ) unwrap_int wrap_int);
    (* Nativeint *)
    ("%nativeint_neg", prim1 Int64.neg unwrap_int64 wrap_int64);
    ("%nativeint_add", prim2 Int64.add unwrap_int64 unwrap_int64 wrap_int64);
    ("%nativeint_sub", prim2 Int64.sub unwrap_int64 unwrap_int64 wrap_int64);
    ("%nativeint_mul", prim2 Int64.mul unwrap_int64 unwrap_int64 wrap_int64);
    ("%nativeint_div", prim2 Int64.div unwrap_int64 unwrap_int64 wrap_int64);
    ("%nativeint_mod", prim2 Int64.rem unwrap_int64 unwrap_int64 wrap_int64);
    ("%nativeint_and", prim2 Int64.logand unwrap_int64 unwrap_int64 wrap_int64);
    ("%nativeint_or", prim2 Int64.logor unwrap_int64 unwrap_int64 wrap_int64);
    ("%nativeint_xor", prim2 Int64.logxor unwrap_int64 unwrap_int64 wrap_int64);
    ( "%nativeint_lsl",
      prim2 Int64.shift_left unwrap_int64 unwrap_int wrap_int64 );
    ( "%nativeint_lsr",
      prim2 Int64.shift_right_logical unwrap_int64 unwrap_int wrap_int64 );
    ( "%nativeint_asr",
      prim2 Int64.shift_right unwrap_int64 unwrap_int wrap_int64 );
    ("%nativeint_of_int", prim1 Int64.of_int unwrap_int wrap_int64);
    ("%nativeint_to_int", prim1 Int64.to_int unwrap_int64 wrap_int);
    ("caml_nativeint_of_string", prim1 Int64.of_string unwrap_string wrap_int64);
    (* Array *)
    ("caml_make_vect", prim2 Array.make unwrap_int id wrap_array_id);
    ("%array_length", prim1 Array.length unwrap_array_id wrap_int);
    ( "caml_array_sub",
      prim3 Array.sub unwrap_array_id unwrap_int unwrap_int wrap_array_id );
    ("%array_safe_get", prim2 Array.get unwrap_array_id unwrap_int id);
    ("%array_unsafe_get", prim2 Array.unsafe_get unwrap_array_id unwrap_int id);
    ("%array_safe_set", prim3 Array.set unwrap_array_id unwrap_int id wrap_unit);
    ( "%array_unsafe_set",
      prim3 Array.unsafe_set unwrap_array_id unwrap_int id wrap_unit );
    ( "caml_array_blit",
      prim5
        Array.blit
        unwrap_array_id
        unwrap_int
        unwrap_array_id
        unwrap_int
        unwrap_int
        wrap_unit );
    ( "caml_array_append",
      prim2 append_prim unwrap_array_id unwrap_array_id wrap_array_id );
    (* Hashtbl *)
    ( "caml_hash",
      prim4 seeded_hash_param unwrap_int unwrap_int unwrap_int id wrap_int );
    (* TODO: records defined in different order... *)

    (* Weak *)
    ( "caml_weak_create",
      prim1
        (fun n -> Array.make n (Constructor ("None", 0, None)))
        unwrap_int
        wrap_array_id );
    ("caml_weak_get", prim2 (fun a n -> a.(n)) unwrap_array_id unwrap_int id);
    ( "caml_weak_get_copy",
      prim2 (fun a n -> a.(n)) unwrap_array_id unwrap_int id );
    ( "caml_weak_set",
      prim3 (fun a n v -> a.(n) <- v) unwrap_array_id unwrap_int id wrap_unit
    );
    ( "caml_weak_check",
      prim2
        (fun a n -> a.(n) <> Constructor ("None", 0, None))
        unwrap_array_id
        unwrap_int
        wrap_bool );
    ( "caml_weak_blit",
      prim5
        Array.blit
        unwrap_array_id
        unwrap_int
        unwrap_array_id
        unwrap_int
        unwrap_int
        wrap_unit );
    (* Random *)
    ( "caml_sys_random_seed",
      prim1 random_seed unwrap_unit (wrap_array wrap_int) );
    (* Gc *)
    ("caml_gc_quick_stat", prim1 Gc.quick_stat unwrap_unit wrap_gc_stat);
    (* Digest *)
    ( "caml_md5_string",
      prim3
        digest_unsafe_string
        unwrap_string
        unwrap_int
        unwrap_int
        wrap_string );
    ( "caml_md5_chan",
      prim2 Digest.channel unwrap_in_channel unwrap_int wrap_string );
    (* Ugly *)
    ( "%obj_size",
      prim1
        (function
          | Array a -> Array.length a + 2
          | _ -> 4)
        id
        wrap_int );
    ( "caml_obj_block",
      prim2
        (fun tag n -> Constructor ("", tag, Some (Tuple [])))
        unwrap_int
        unwrap_int
        id )
  ]

let prims =
  List.fold_left (fun env (name, v) -> SMap.add name v env) SMap.empty prims

let rec expr_label_shape = function
  | Pexp_fun (label, default, _, e) ->
    (label, default) :: expr_label_shape e.pexp_desc
  | Pexp_function _ -> [ (Nolabel, None) ]
  | _ -> []

let fun_label_shape = function
  | Fun (lab, default, _, e, _) ->
    (lab, default) :: expr_label_shape e.pexp_desc
  | Function _ -> [ (Nolabel, None) ]
  | Prim _ -> [ (Nolabel, None) ]
  | SeqOr | SeqAnd -> [ (Nolabel, None); (Nolabel, None) ]
  | _ -> []

(*
let rec expr_num_args = function
  | Pexp_fun (_, _, _, e) -> 1 + expr_num_args e.pexp_desc
  | Pexp_function _ -> 1
  | _ -> 0

let rec fun_num_args = function
  | Fun (_, _, _, e, _) -> 1 + expr_num_args e.pexp_desc
  | Function _ -> 1
  | Prim _ -> 1
  | SeqOr | SeqAnd -> 2
  | Fun_with_extra_args (f, l, m) -> fun_num_args f - List.length l - SMap.cardinal m
  | _ -> 0
*)

let fmt_ebb_of_string_fct = ref (Int 0)

let rec apply vf args =
  let vf, extral, extram =
    match vf with
    | Fun_with_extra_args (vf, extral, extram) -> (vf, extral, extram)
    | _ -> (vf, [], SMap.empty)
  in
  assert (extral = []);
  (* let ls = fun_label_shape vf in *)
  let apply_labelled vf (lab, arg) =
    match vf with
    | Fun (label, default, p, e, fenv) ->
      (match (label, lab, default) with
      | Optional s, Labelled s', None ->
        assert (s = s');
        eval_expr (pattern_bind !fenv p (Constructor ("Some", 0, Some arg))) e
      | Optional s, Labelled s', Some _
      | Optional s, Optional s', None
      | Labelled s, Labelled s', None ->
        assert (s = s');
        eval_expr (pattern_bind !fenv p arg) e
      | Optional s, Optional s', Some def ->
        assert (s = s');
        let arg =
          match arg with
          | Constructor ("None", 0, None) -> eval_expr !fenv def
          | Constructor ("Some", 0, Some arg) -> arg
          | _ -> assert false
        in
        eval_expr (pattern_bind !fenv p arg) e
      | _ -> assert false)
    | _ -> assert false
  in
  let apply_optional_noarg vf =
    match vf with
    | Fun (Optional _, None, p, e, fenv) ->
      eval_expr (pattern_bind !fenv p (Constructor ("None", 0, None))) e
    | Fun (Optional _, Some def, p, e, fenv) ->
      eval_expr (pattern_bind !fenv p (eval_expr !fenv def)) e
    | _ -> assert false
  in
  let unlabelled =
    List.map snd (List.filter (fun (lab, _) -> lab = Nolabel) args)
  in
  let with_label =
    ref
      (List.fold_left
         (fun wl (lab, arg) ->
           match lab with
           | Nolabel -> wl
           | Optional s | Labelled s -> SMap.add s (lab, arg) wl)
         extram
         args)
  in
  let has_labelled = not (SMap.is_empty !with_label) in
  let rec apply_one vf arg =
    match vf with
    | Fun (Nolabel, default, p, e, fenv) ->
      eval_expr (pattern_bind !fenv p arg) e
    | Fun (((Labelled s | Optional s) as lab), default, p, e, fenv) ->
      if has_labelled
      then
        if SMap.mem s !with_label
        then (
          let v = SMap.find s !with_label in
          with_label := SMap.remove s !with_label;
          apply_one (apply_labelled vf v) arg)
        else (
          assert (lab = Optional s);
          apply_one (apply_optional_noarg vf) arg)
      else if lab = Optional s
      then apply_one (apply_optional_noarg vf) arg
      else eval_expr (pattern_bind !fenv p arg) e
    | Function (cl, fenv) -> eval_match !fenv cl (Ok arg)
    | Prim prim -> prim arg
    | SeqOr ->
      if is_true arg then Prim (fun _ -> wrap_bool true) else Prim (fun x -> x)
    | SeqAnd ->
      if is_true arg then Prim (fun x -> x) else Prim (fun _ -> wrap_bool false)
    | v ->
      Format.eprintf "%a@." pp_print_value v;
      assert false
  in
  if SMap.is_empty !with_label
  then
    (* Special case to get tail recursion *)
    List.fold_left apply_one vf unlabelled
  else (
    let vf = List.fold_left apply_one vf unlabelled in
    let rec apply_loop vf =
      if SMap.is_empty !with_label
      then vf
      else (
        match vf with
        | Fun (((Labelled s | Optional s) as lab), default, p, e, fenv) ->
          if SMap.mem s !with_label
          then (
            let v = SMap.find s !with_label in
            with_label := SMap.remove s !with_label;
            apply_loop (apply_labelled vf v))
          else (
            assert (lab = Optional s);
            apply_loop (apply_optional_noarg vf))
        | _ -> Fun_with_extra_args (vf, [], !with_label))
    in
    apply_loop vf)

and eval_expr env expr =
  match expr.pexp_desc with
  | Pexp_ident id -> env_get_value env id
  | Pexp_constant c -> value_of_constant c
  | Pexp_let (f, vals, e) ->
    if f = Nonrecursive
    then (
      let nenv = List.fold_left (bind_value env) env vals in
      eval_expr nenv e)
    else (
      let er = ref env in
      let nenv = List.fold_left (bind_value_rec er) env vals in
      er := nenv;
      eval_expr nenv e)
  | Pexp_function cl -> Function (cl, ref env)
  | Pexp_fun (label, default, p, e) -> Fun (label, default, p, e, ref env)
  | Pexp_apply (f, l) ->
    let fc = eval_expr env f in
    (match (fc, l) with
    | SeqOr, [ (_, arg1); (_, arg2) ] ->
      let a1 = eval_expr env arg1 in
      if is_true a1 then wrap_bool true else eval_expr env arg2
    | SeqAnd, [ (_, arg1); (_, arg2) ] ->
      let a1 = eval_expr env arg1 in
      if is_true a1 then eval_expr env arg2 else wrap_bool false
    | _ ->
      let args = List.map (fun (lab, e) -> (lab, eval_expr env e)) l in
      if trace
      then (
        match f.pexp_desc with
        | Pexp_ident lident ->
          Format.eprintf
            "apply %s"
            (String.concat "." (Longident.flatten lident.txt));
          incr tracecur;
          if !tracecur > tracearg_from
          then
            Format.eprintf
              " %a"
              (Format.pp_print_list
                 ~pp_sep:(fun ff () -> Format.fprintf ff " ")
                 (fun ff (_, v) -> Format.fprintf ff "%a" pp_print_value v))
              args;
          Format.eprintf "@."
        | _ -> ());
      (match f.pexp_desc with
      | Pexp_ident lident when lident_name lident.txt = "yyparse" ->
        cur_env := env
      | _ -> ());
      (*Hack for parsing.c*)
      apply fc args)
  | Pexp_tuple l ->
    let args = List.map (eval_expr env) l in
    Tuple args
  | Pexp_match (e, cl) -> eval_match env cl (eval_expr_exn env e)
  | Pexp_coerce (e, _, _) -> eval_expr env e
  | Pexp_constraint (e, _) -> eval_expr env e
  | Pexp_sequence (e1, e2) ->
    let _ = eval_expr env e1 in
    eval_expr env e2
  | Pexp_while (e1, e2) ->
    while is_true (eval_expr env e1) do
      ignore (eval_expr env e2)
    done;
    unit
  | Pexp_for (p, e1, e2, flag, e3) ->
    let v1 =
      match eval_expr env e1 with
      | Int n -> n
      | _ -> assert false
    in
    let v2 =
      match eval_expr env e2 with
      | Int n -> n
      | _ -> assert false
    in
    if flag = Upto
    then
      for x = v1 to v2 do
        ignore (eval_expr (pattern_bind env p (Int x)) e3)
      done
    else
      for x = v1 downto v2 do
        ignore (eval_expr (pattern_bind env p (Int x)) e3)
      done;
    unit
  | Pexp_ifthenelse (e1, e2, e3) ->
    if is_true (eval_expr env e1)
    then eval_expr env e2
    else (
      match e3 with
      | None -> unit
      | Some e3 -> eval_expr env e3)
  | Pexp_unreachable -> failwith "reached unreachable"
  | Pexp_try (e, cs) ->
    (try eval_expr env e
     with InternalException v ->
       (try eval_match env cs (Ok v)
        with Match_fail -> raise (InternalException v)))
  | Pexp_construct (c, e) ->
    let cn = lident_name c.txt in
    let d = env_get_constr env c in
    let ee =
      match e with
      | None -> None
      | Some e -> Some (eval_expr env e)
    in
    Constructor (cn, d, ee)
  | Pexp_variant (cn, e) ->
    let ee =
      match e with
      | None -> None
      | Some e -> Some (eval_expr env e)
    in
    Constructor (cn, Hashtbl.hash cn, ee)
  | Pexp_record (r, e) ->
    let base =
      match e with
      | None -> SMap.empty
      | Some e ->
        (match eval_expr env e with
        | Record r -> r
        | _ -> assert false)
    in
    Record
      (List.fold_left
         (fun rc ({ txt = lident }, ee) ->
           SMap.add (lident_name lident) (ref (eval_expr env ee)) rc)
         base
         r)
  | Pexp_field (e, { txt = lident }) ->
    (match eval_expr env e with
    | Record r -> !(SMap.find (lident_name lident) r)
    | _ -> assert false)
  | Pexp_setfield (e1, { txt = lident }, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
    | Record r ->
      SMap.find (lident_name lident) r := v2;
      unit
    | _ -> assert false)
  | Pexp_array l -> Array (Array.of_list (List.map (eval_expr env) l))
  | Pexp_send _ -> assert false
  | Pexp_new _ -> assert false
  | Pexp_setinstvar _ -> assert false
  | Pexp_override _ -> assert false
  | Pexp_letexception ({ pext_name = name; pext_kind = k }, e) ->
    let nenv =
      match k with
      | Pext_decl _ ->
        let d = !exn_id in
        incr exn_id;
        env_set_constr name.txt d env
      | Pext_rebind path ->
        env_set_constr name.txt (env_get_constr env path) env
    in
    eval_expr nenv e
  | Pexp_letmodule (name, me, e) ->
    let m = eval_module_expr env me in
    eval_expr (env_set_module name.txt m env) e
  | Pexp_assert e ->
    if is_true (eval_expr env e)
    then unit
    else
      (*failwith "assert failure"*)
      raise
        (InternalException
           (Constructor
              ( "Assert_failure",
                assert_failure_id,
                Some (Tuple [ wrap_string ""; Int 0; Int 0 ]) )))
  | Pexp_lazy e -> Lz (ref (fun () -> eval_expr env e))
  | Pexp_poly _ -> assert false
  | Pexp_newtype (_, e) -> eval_expr env e
  | Pexp_open (_, lident, e) ->
    let nenv =
      match env_get_module env lident with
      | Module (venv, menv, cenv) -> env_extend false env (venv, menv, cenv)
      | Functor _ -> assert false
      | exception Not_found -> env
      (* Module might be a .mli only *)
    in
    eval_expr nenv e
  | Pexp_object _ -> assert false
  | Pexp_pack me -> ModVal (eval_module_expr env me)
  | Pexp_extension _ -> assert false

and eval_expr_exn env expr =
  try Ok (eval_expr env expr) with InternalException v -> Error v

and bind_value evalenv bindenv vb =
  let v = eval_expr evalenv vb.pvb_expr in
  pattern_bind bindenv vb.pvb_pat v

and bind_value_rec evalenvref bindenv vb =
  let v = eval_fun_or_function evalenvref vb.pvb_expr in
  pattern_bind bindenv vb.pvb_pat v

and pattern_bind env pat v =
  match pat.ppat_desc with
  | Ppat_any -> env
  | Ppat_var s -> env_set_value s.txt v env
  | Ppat_alias (p, s) -> env_set_value s.txt v (pattern_bind env p v)
  | Ppat_constant c ->
    if value_equal (value_of_constant c) v then env else raise Match_fail
  | Ppat_interval (c1, c2) ->
    if value_le (value_of_constant c1) v && value_le v (value_of_constant c2)
    then env
    else raise Match_fail
  | Ppat_tuple l ->
    (match v with
    | Tuple vl ->
      assert (List.length l = List.length vl);
      List.fold_left2 pattern_bind env l vl
    | _ -> assert false)
  | Ppat_construct (c, p) ->
    let cn = lident_name c.txt in
    let dn = env_get_constr env c in
    (match v with
    | Constructor (ccn, ddn, e) ->
      if cn <> ccn then raise Match_fail;
      if dn <> ddn then raise Match_fail;
      (match (p, e) with
      | None, None -> env
      | Some p, Some e -> pattern_bind env p e
      | _ -> assert false)
    | String s ->
      assert (lident_name c.txt = "Format");
      let p =
        match p with
        | None -> assert false
        | Some p -> p
      in
      let fmt = apply !fmt_ebb_of_string_fct [ (Nolabel, String s) ] in
      let fmt =
        match fmt with
        | Constructor ("Fmt_EBB", _, Some fmt) -> fmt
        | _ -> assert false
      in
      pattern_bind env p (Tuple [ fmt; v ])
    | _ ->
      Format.eprintf "cn = %s@.v = %a@." cn pp_print_value v;
      assert false)
  | Ppat_variant (name, p) ->
    (match v with
    | Constructor (cn, _, e) ->
      if cn <> name then raise Match_fail;
      (match (p, e) with
      | None, None -> env
      | Some p, Some e -> pattern_bind env p e
      | _ -> assert false)
    | _ -> assert false)
  | Ppat_record (rp, _) ->
    (match v with
    | Record r ->
      List.fold_left
        (fun env (lident, p) ->
          pattern_bind env p !(SMap.find (lident_name lident.txt) r))
        env
        rp
    | _ -> assert false)
  | Ppat_array _ -> assert false
  | Ppat_or (p1, p2) ->
    (try pattern_bind env p1 v with Match_fail -> pattern_bind env p2 v)
  | Ppat_constraint (p, _) -> pattern_bind env p v
  | Ppat_type _ -> assert false
  | Ppat_lazy _ -> assert false
  | Ppat_unpack name ->
    (match v with
    | ModVal m -> env_set_module name.txt m env
    | _ -> assert false)
  | Ppat_exception _ -> raise Match_fail
  | Ppat_extension _ -> assert false
  | Ppat_open _ -> assert false

and pattern_bind_exn env pat v =
  match pat.ppat_desc with
  | Ppat_exception p -> pattern_bind env p v
  | _ -> raise Match_fail

and pattern_bind_checkexn env pat v =
  match v with
  | Ok v -> pattern_bind env pat v
  | Error v -> pattern_bind_exn env pat v

and eval_match env cl arg =
  match cl with
  | [] ->
    (match arg with
    | Ok _ -> raise Match_fail
    | Error v -> raise (InternalException v))
  | c :: cl ->
    (match pattern_bind_checkexn env c.pc_lhs arg with
    | exception Match_fail -> eval_match env cl arg
    | nenv ->
      let guard_ok =
        match c.pc_guard with
        | None -> true
        | Some guard -> is_true (eval_expr nenv guard)
      in
      if guard_ok then eval_expr nenv c.pc_rhs else eval_match env cl arg)

and eval_module_expr env me =
  match me.pmod_desc with
  | Pmod_ident lident -> env_get_module env lident
  | Pmod_structure str -> make_module (eval_structure None env str)
  | Pmod_functor ({ txt = arg_name }, _, e) -> Functor (arg_name, e, env)
  | Pmod_constraint (me, _) -> eval_module_expr env me
  | Pmod_apply (me1, me2) ->
    let m1 = eval_module_expr env me1 in
    let m2 = eval_module_expr env me2 in
    (match m1 with
    | Module _ -> assert false
    | Functor (arg_name, body, env) ->
      eval_module_expr (env_set_module arg_name m2 env) body)
  | Pmod_unpack e ->
    (match eval_expr env e with
    | ModVal m -> m
    | _ -> assert false)
  | Pmod_extension _ -> assert false

and eval_structitem init_ignored env it =
  match it.pstr_desc with
  | Pstr_eval (e, _) ->
    let v = eval_expr env e in
    Format.printf "%a@." pp_print_value v;
    env
  | Pstr_value (f, vals) ->
    if f = Nonrecursive
    then List.fold_left (bind_value env) env vals
    else (
      let er = ref env in
      let nenv = List.fold_left (bind_value_rec er) env vals in
      er := nenv;
      nenv)
  | Pstr_primitive { pval_name = { txt = name; loc }; pval_prim = l } ->
    let prim_name = List.hd l in
    let prim =
      try SMap.find prim_name prims
      with Not_found ->
        Prim (fun _ ->
            if debug then Format.eprintf "%a@." Location.print_loc loc;
            failwith ("Unimplemented primitive " ^ prim_name))
    in
    env_set_value name prim env
  | Pstr_type (_, tl) ->
    List.fold_left
      (fun env t ->
        match t.ptype_kind with
        | Ptype_variant l ->
          let _, _, env =
            List.fold_left
              (fun (u, v, env) cd ->
                match cd.pcd_args with
                | Pcstr_tuple [] ->
                  (u + 1, v, env_set_constr cd.pcd_name.txt u env)
                | _ -> (u, v + 1, env_set_constr cd.pcd_name.txt v env))
              (0, 0, env)
              l
          in
          env
        | _ -> env)
      env
      tl
  | Pstr_typext _ -> env
  | Pstr_exception { pext_name = name; pext_kind = k } ->
    (match k with
    | Pext_decl _ ->
      let d = !exn_id in
      incr exn_id;
      env_set_constr name.txt d env
    | Pext_rebind path -> env_set_constr name.txt (env_get_constr env path) env)
  | Pstr_module { pmb_name = name; pmb_expr = me } ->
    (match init_ignored with
    | None -> env_set_module name.txt (eval_module_expr env me) env
    | Some ign ->
      (try env_set_module name.txt (eval_module_expr env me) env
       with Not_found ->
         assert (
           match me.pmod_desc with
           | Pmod_ident { txt = Longident.Lident s } -> s = name.txt
           | _ -> false);
         ign := SSet.add name.txt !ign;
         env))
  | Pstr_recmodule _ -> assert false
  | Pstr_modtype _ -> env
  | Pstr_open { popen_lid = lident } ->
    (match env_get_module env lident with
    | Module (venv, menv, cenv) -> env_extend false env (venv, menv, cenv)
    | Functor _ -> assert false)
  | Pstr_class _ -> assert false
  | Pstr_class_type _ -> assert false
  | Pstr_include { pincl_mod = me } ->
    let m = eval_module_expr env me in
    (match m with
    | Module (venv, menv, cenv) -> env_extend true env (venv, menv, cenv)
    | Functor _ -> assert false)
  | Pstr_attribute _ -> env
  | Pstr_extension _ -> assert false

and eval_structure_ init_ignored env str =
  match str with
  | [] -> env
  | it :: str ->
    eval_structure_ init_ignored (eval_structitem init_ignored env it) str

and eval_structure init_ignored env str =
  eval_structure_ init_ignored (prevent_export env) str

(*
and eval_sigitem_noimpl env = function
  | Psig_attribute _ -> env
  | Psig_class _ -> assert false
  | Psig_class_type _ -> assert false
  | Psig_exception { pext_name = { txt = name } ; pext_kind = k } ->
    begin
      match k with
      | Pext_decl _ -> let d = !exn_id in incr exn_id; env_set_constr name d env
      | Pext_rebind { txt = path } -> env_set_constr name (env_get_constr env path) env
    end
  | Psig_extension _ -> assert false
  | Psig_include { pincl_mod = mt } ->
    let m = eval_module_type env mt in
    (match m with
     | Module (venv, menv, cenv) -> env_extend true env (venv, menv, cenv)
     | Functor _ -> assert false)
  | Psig_open { popen_lid = { txt = lident } } ->
    (match env_get_module env lident with
     | Module (venv, menv, cenv) -> env_extend false env (venv, menv, cenv)
     | Functor _ -> assert false)
  | Psig_value z -> assert false (* load mlis without implementation only *)
  | Psig_module _ -> assert false (* TODO *)
  | Psig_modtype _ -> assert false
  | Psig_type (_, tl) ->
    List.fold_left (fun env t ->
        match t.ptype_kind with
        | Ptype_variant l ->
          let (_, _, env) = List.fold_left (fun (u, v, env) cd ->
              match cd.pcd_args with
              | Pcstr_tuple [] -> (u + 1, v, env_set_constr cd.pcd_name.txt u env)
              | _ -> (u, v + 1, env_set_constr cd.pcd_name.txt v env)
            ) (0, 0, env) l in
          env
        | _ -> env
      ) env tl
  | Psig_typext _ -> assert false
  | Psig_recmodule _ -> assert false

and eval_module_type env mt =
  match mt.pmty_desc with
  | Pmty_ident { txt = lident } -> env_get_module env lident
  | Pmty_signature sg -> make_module (eval_signature_noimpl env sg)
  | Pmty_functor ({ txt = argname }, input_type, result) -> (* hope it doesn't happen *) assert false
  | Pmty_with _ -> assert false
  | Pmty_typeof _ -> assert false
  | Pmty_alias _ -> assert false
  | Pmty_extension _ -> assert false

and eval_signature_noimpl env = function
  | [] -> env
  | it :: sg -> eval_signature_noimpl (eval_sigitem_noimpl env it.psig_desc) sg
*)

let () = apply_ref := apply

let parse filename =
  let inc = open_in filename in
  let lexbuf = Lexing.from_channel inc in
  Location.init lexbuf filename;
  let parsed = Parse.implementation lexbuf in
  close_in inc;
  parsed

let z x = x

let stdlib_modules =
  [ ("Sys", "sys.ml", z);
    ("Seq", "seq.ml", z);
    ("List", "list.ml", z);
    ("Set", "set.ml", z);
    ("Map", "map.ml", z);
    ("Char", "char.ml", z);
    ("Bytes", "bytes.ml", z);
    ("String", "string.ml", z);
    ("Buffer", "buffer.ml", z);
    ("CamlinternalFormatBasics", "camlinternalFormatBasics.ml", z);
    ( "CamlinternalFormat",
      "camlinternalFormat.ml",
      fun env ->
        fmt_ebb_of_string_fct :=
          env_get_value
            env
            (Location.mknoloc (Longident.Lident "fmt_ebb_of_string"));
        env );
    ("Printf", "printf.ml", z);
    ("Format", "format.ml", z);
    ("Obj", "obj.ml", z);
    ("Gc", "gc.ml", z);
    ("CamlinternalLazy", "camlinternalLazy.ml", z);
    ("Lazy", "lazy.ml", z);
    ("Array", "array.ml", z);
    ("Int64", "int64.ml", z);
    ("Int32", "int32.ml", z);
    ("Nativeint", "nativeint.ml", z);
    ("Digest", "digest.ml", z);
    ("Random", "random.ml", z);
    ("Hashtbl", "hashtbl.ml", z);
    ("Lexing", "lexing.ml", z);
    ("Parsing", "parsing.ml", z);
    ("Weak", "weak.ml", z);
    ("Stack", "stack.ml", z);
    ("Arg", "arg.ml", z);
    ("Filename", "filename.ml", z);
    ("CamlinternalOO", "camlinternalOO.ml", z);
    ("Marshal", "marshal.ml", z)
  ]

let stdlib_path =
  match Sys.getenv_opt "OCAMLINTERP_STDLIB_PATH" with
  | Some path -> path
  | None ->
    let input = Unix.open_process_in "ocamlc -where" in
    (match input_line input with
    | exception _ ->
      close_in input;
      failwith "Error: unable to determine the standard library location"
    | path ->
      close_in input;
      path)

let stdlib_modules =
  List.map
    (fun (n, p, modifier) -> (n, stdlib_path ^ "/" ^ p, modifier))
    stdlib_modules

let load_modules env modules =
  List.fold_left
    (fun env (modname, modpath, modifier) ->
      if debug then Format.eprintf "Loading %s from %s@." modname modpath;
      let module_contents =
        modifier (eval_structure None env (parse modpath))
      in
      env_set_module modname (make_module module_contents) env)
    env
    modules

let init_env =
  let stdlib_main = parse (stdlib_path ^ "/stdlib.ml") in
  let ign = ref SSet.empty in
  let env = eval_structure (Some ign) !initial_env stdlib_main in
  let env = load_modules env stdlib_modules in
  env_set_module "Stdlib" (make_module env) env

let compiler_modules =
  [ (* Utils *)
    ("Config", "utils/config.ml", z);
    ("Misc", "utils/misc.ml", z);
    ("Identifiable", "utils/identifiable.ml", z);
    ("Numbers", "utils/numbers.ml", z);
    ("Arg_helper", "utils/arg_helper.ml", z);
    ("Clflags", "utils/clflags.ml", z);
    ("Tbl", "utils/tbl.ml", z);
    ("Profile", "utils/profile.ml", z);
    ("Terminfo", "utils/terminfo.ml", z);
    ("Ccomp", "utils/ccomp.ml", z);
    ("Warnings", "utils/warnings.ml", z);
    ("Consistbl", "utils/consistbl.ml", z);
    ( "Strongly_connected_components",
      "utils/strongly_connected_components.ml",
      z );
    ("Build_path_prefix_map", "utils/build_path_prefix_map.ml", z);
    ("Targetint", "utils/targetint.ml", z);
    (* Parsing *)
    ("Asttypes", "parsing/asttypes.mli", z);
    ("Location", "parsing/location.ml", z);
    ("Longident", "parsing/longident.ml", z);
    ("Parsetree", "parsing/parsetree.mli", z);
    ("Docstrings", "parsing/docstrings.ml", z);
    ("Syntaxerr", "parsing/syntaxerr.ml", z);
    ("Ast_helper", "parsing/ast_helper.ml", z);
    ("Parser", "parsing/parser.ml", z);
    ("Lexer", "parsing/lexer.ml", z);
    ("Parse", "parsing/parse.ml", z);
    ("Printast", "parsing/printast.ml", z);
    ("Pprintast", "parsing/pprintast.ml", z);
    ("Ast_mapper", "parsing/ast_mapper.ml", z);
    ("Ast_iterator", "parsing/ast_iterator.ml", z);
    ("Attr_helper", "parsing/attr_helper.ml", z);
    ("Builtin_attributes", "parsing/builtin_attributes.ml", z);
    ("Ast_invariants", "parsing/ast_invariants.ml", z);
    ("Depend", "parsing/depend.ml", z);
    (* Typing *)
    ("Ident", "typing/ident.ml", z);
    ("Outcometree", "typing/outcometree.mli", z);
    ("Annot", "typing/annot.mli", z);
    ("Path", "typing/path.ml", z);
    ("Primitive", "typing/primitive.ml", z);
    ("Types", "typing/types.ml", z);
    ("Btype", "typing/btype.ml", z);
    ("Oprint", "typing/oprint.ml", z);
    ("Subst", "typing/subst.ml", z);
    ("Predef", "typing/predef.ml", z);
    ("Datarepr", "typing/datarepr.ml", z);
    ("Cmi_format", "typing/cmi_format.ml", z);
    ("Env", "typing/env.ml", z);
    ("Typedtree", "typing/typedtree.ml", z);
    ("Printtyped", "typing/printtyped.ml", z);
    ("Ctype", "typing/ctype.ml", z);
    ("Printtyp", "typing/printtyp.ml", z);
    ("Includeclass", "typing/includeclass.ml", z);
    ("Mtype", "typing/mtype.ml", z);
    ("Envaux", "typing/envaux.ml", z);
    ("Includecore", "typing/includecore.ml", z);
    ("TypedtreeIter", "typing/typedtreeIter.ml", z);
    ("TypedtreeMap", "typing/typedtreeMap.ml", z);
    ("Tast_mapper", "typing/tast_mapper.ml", z);
    ("Cmt_format", "typing/cmt_format.ml", z);
    ("Untypeast", "typing/untypeast.ml", z);
    ("Includemod", "typing/includemod.ml", z);
    ("Typetexp", "typing/typetexp.ml", z);
    ("Printpat", "typing/printpat.ml", z);
    ("Parmatch", "typing/parmatch.ml", z);
    ("Stypes", "typing/stypes.ml", z);
    ("Typedecl", "typing/typedecl.ml", z);
    (* Comp *)
    ("Lambda", "bytecomp/lambda.ml", z);
    (* Typing *)
    ("Typeopt", "typing/typeopt.ml", z);
    ("Typecore", "typing/typecore.ml", z);
    ("Typeclass", "typing/typeclass.ml", z);
    ("Typemod", "typing/typemod.ml", z);
    (* Comp *)
    ("Cmo_format", "bytecomp/cmo_format.mli", z);
    ("Printlambda", "bytecomp/printlambda.ml", z);
    ("Semantics_of_primitives", "bytecomp/semantics_of_primitives.ml", z);
    ("Switch", "bytecomp/switch.ml", z);
    ("Matching", "bytecomp/matching.ml", z);
    ("Translobj", "bytecomp/translobj.ml", z);
    ("Translattribute", "bytecomp/translattribute.ml", z);
    ("Translprim", "bytecomp/translprim.ml", z);
    ("Translcore", "bytecomp/translcore.ml", z);
    ("Translclass", "bytecomp/translclass.ml", z);
    ("Translmod", "bytecomp/translmod.ml", z);
    ("Simplif", "bytecomp/simplif.ml", z);
    ("Runtimedef", "bytecomp/runtimedef.ml", z);
    ("Meta", "bytecomp/meta.ml", z);
    ("Opcodes", "bytecomp/opcodes.ml", z);
    ("Bytesections", "bytecomp/bytesections.ml", z);
    ("Dll", "bytecomp/dll.ml", z);
    ("Symtable", "bytecomp/symtable.ml", z);
    ("Pparse", "driver/pparse.ml", z);
    ("Main_args", "driver/main_args.ml", z);
    ("Compenv", "driver/compenv.ml", z);
    ("Compmisc", "driver/compmisc.ml", z);
    ("Compdynlink", "driver/compdynlink.mlno", z);
    ("Compplugin", "driver/compplugin.ml", z);
    ("Makedepend", "driver/makedepend.ml", z);
    (* Bytecomp *)
    ("Instruct", "bytecomp/instruct.ml", z);
    ("Bytegen", "bytecomp/bytegen.ml", z);
    ("Printinstr", "bytecomp/printinstr.ml", z);
    ("Emitcode", "bytecomp/emitcode.ml", z);
    ("Bytelink", "bytecomp/bytelink.ml", z);
    ("Bytelibrarian", "bytecomp/bytelibrarian.ml", z);
    ("Bytepackager", "bytecomp/bytepackager.ml", z);
    ("Errors", "driver/errors.ml", z);
    ("Compile", "driver/compile.ml", z);
    (* Bytestart *)
    ("Main", "driver/main.ml", z)
  ]

let compiler_source_path =
  match Sys.getenv_opt "OCAMLINTERP_SRC_PATH" with
  | Some path -> path
  | None ->
    failwith
      "Error: please set an OCAMLINTERP_SRC_PATH variable pointing to a \
       checkout of the OCaml compiler distribution sources"

let compiler_modules =
  List.map
    (fun (n, p, modifier) -> (n, compiler_source_path ^ "/" ^ p, modifier))
    compiler_modules

(* let _ = eval_structure None init_env parsed *)
let () =
  try ignore (load_modules init_env compiler_modules)
  with InternalException e ->
    Format.eprintf "Code raised exception: %a@." pp_print_value e
