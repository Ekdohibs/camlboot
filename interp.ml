open Asttypes
open Parsetree

let trace = false
let tracearg_from = 742740000
let tracecur = ref 0
let debug = false

module SMap = Map.Make(String)
module SSet = Set.Make(String)

let tag_Fun = 230
let tag_Function = 231
let tag_ModVal = 232
let tag_SeqOr = 233
let tag_SeqAnd = 234
let tag_Lz = 235
let tag_Lz_computed = 236
let tag_Fun_with_extra_args = 237
let tag_Prim = 238

(*
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
  | SeqOr | SeqAnd
  | Lz of (unit -> value) ref
  | Array of value array
  | Fun_with_extra_args of value * value list * (arg_label * value) SMap.t
*)

type value = Obj.t

and env = {
  env_vars : (bool * value) SMap.t ;
  env_modules : (bool * mdl) SMap.t ;
  (* tag, description, is_exception *)
  env_constructors : (bool * (int * constr_desc * bool)) SMap.t ;
  (* is_static, field id, field ids of all fields in the record *)
  (* static records do not include layout information, and are as such suitable for
     marshalling and passing to primitives.
     However, they can have bugs with type-based disambiguation if there are other fields with
     the same name in other records
  *)
  env_fields : (bool * (bool * int * int SMap.t)) SMap.t ;
}

and constr_desc =
  | CTuple of int (* arity *)
  | CRecord of string list * int SMap.t

and mdl =
  | Module of value SMap.t * mdl SMap.t * (int * constr_desc * bool) SMap.t * (bool * int * int SMap.t) SMap.t
  | Functor of string * module_expr * env (* TODO: include arg restriction *)

exception InternalException of value

(*
let rec pp_print_value ff = function
  | Int n -> Format.fprintf ff "%d" n
  | Int64 n -> Format.fprintf ff "%Ld" n
  | Fun _ | Function _ | Prim _ | SeqOr | SeqAnd | Lz _ | Fun_with_extra_args _ -> Format.fprintf ff "<function>"
  | String s -> Format.fprintf ff "%S" (Bytes.to_string s)
  | Float f -> Format.fprintf ff "%f" f
  | Tuple l -> Format.fprintf ff "(%a)" (Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff ", ") pp_print_value) l
  | Constructor (c, d, None) -> Format.fprintf ff "%s#%d" c d
  | Constructor (c, d, Some v) -> Format.fprintf ff "%s#%d %a" c d pp_print_value v
  | ModVal _ -> Format.fprintf ff "<module>"
  | InChannel _ -> Format.fprintf ff "<in_channel>"
  | OutChannel _ -> Format.fprintf ff "<out_channel>"
  | Record r -> Format.fprintf ff "{"; SMap.iter (fun k v -> Format.fprintf ff "%s = %a; " k pp_print_value !v) r; Format.fprintf ff "}"
  | Array a -> Format.fprintf ff "[|%a|]" (Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff "; ") pp_print_value) (Array.to_list a)
*)

let read_caml_int s =
  let c = ref 0L in
  let sign, init = if String.length s > 0 && s.[0] = '-' then (-1L, 1) else (1L, 0) in
  let base, init =
    if String.length s >= init + 2 && s.[init] = '0' then
      ((match s.[init + 1] with 'x' | 'X' -> 16L | 'b' | 'B' -> 2L | 'o' | 'O' -> 8L | _ -> assert false), init + 2)
    else
      (10L, init)
  in
  for i = init to String.length s - 1 do
    match s.[i] with
    | '0'..'9' as x -> c := Int64.(add (mul base !c) (of_int (int_of_char x - int_of_char '0')))
    | 'a'..'f' as x -> c := Int64.(add (mul base !c) (of_int (int_of_char x - int_of_char 'a' + 10)))
    | 'A'..'F' as x -> c := Int64.(add (mul base !c) (of_int (int_of_char x - int_of_char 'A' + 10)))
    | '_' -> ()
    | _ -> Format.eprintf "FIXME literal: %s@." s; assert false
  done;
  Int64.mul sign !c

let value_of_constant = function
  | Pconst_integer (s, None) -> Obj.repr (Int64.to_int (read_caml_int s))
  | Pconst_integer (s, Some 'l') -> Obj.repr (Int64.to_int32 (read_caml_int s))
  | Pconst_integer (s, Some 'L') -> Obj.repr (read_caml_int s)
  | Pconst_integer (s, Some 'n') -> Obj.repr (Int64.to_nativeint (read_caml_int s))
  | Pconst_integer (s, Some c) -> Format.eprintf "Unsupported suffix %c@." c; assert false
  | Pconst_char c -> Obj.repr (int_of_char c)
  | Pconst_float (f, _) -> Obj.repr (float_of_string f)
  | Pconst_string (s, _) -> Obj.repr (Bytes.of_string s)

(*
let rec value_equal v1 v2 =
  match v1, v2 with
  | Fun _, _ | Function _, _ | _, Fun _ | _, Function _ | SeqOr, _  | SeqAnd, _ | _, SeqOr | _, SeqAnd | Lz _, _ | _, Lz _ | Fun_with_extra_args _, _ | _, Fun_with_extra_args _ ->
    failwith "tried to compare function"
  | ModVal _, _ | _, ModVal _ ->
    failwith "tried to compare module"
  | InChannel _, _ | OutChannel _, _ | _, InChannel _ | _, OutChannel _ ->
    failwith "tried to compare channel"
  | Int n1, Int n2 -> n1 = n2
  | Int64 n1, Int64 n2 -> n1 = n2
  | Float f1, Float f2 -> f1 = f2
  | String s1, String s2 -> s1 = s2
  | Constructor (c1, d1, None), Constructor (c2, d2, None) -> d1 = d2 && c1 = c2
  | Constructor (c1, d1, Some v1), Constructor (c2, d2, Some v2) -> d1 = d2 && c1 = c2 && value_equal v1 v2
  | Constructor _, Constructor _ -> false
  | Tuple l1, Tuple l2 -> assert (List.length l1 = List.length l2); List.for_all2 value_equal l1 l2
  | Record r1, Record r2 ->
    SMap.for_all (fun _ b -> b) (SMap.merge (fun _ u v -> match u, v with None, None -> None | None, Some _ | Some _, None -> Some false | Some u, Some v -> Some (value_equal !u !v)) r1 r2)
  | Array a1, Array a2 -> if (Array.length a1 <> Array.length a2) then false else (let ok = ref true in for i = 0 to Array.length a1 - 1 do ok := !ok && value_equal a1.(i) a2.(i) done; !ok)
  | _ -> false

let rec value_compare v1 v2 =
  match v1, v2 with
  | Fun _, _ | Function _, _ | _, Fun _ | _, Function _ | SeqOr, _ | SeqAnd, _ | _, SeqOr | _, SeqAnd | Lz _, _ | _, Lz _ | Fun_with_extra_args _, _ | _, Fun_with_extra_args _ ->
    failwith "tried to compare function"
  | ModVal _, _ | _, ModVal _ ->
    failwith "tried to compare module"
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
    if c <> 0 then c else begin
      match vv1, vv2 with
      | None, None -> 0
      | Some v1, Some v2 -> value_compare v1 v2
      | _ -> assert false
    end
  | Tuple l1, Tuple l2 -> assert (List.length l1 = List.length l2);
    List.fold_left2 (fun cur x y -> if cur = 0 then value_compare x y else cur) 0 l1 l2
  | Record r1, Record r2 ->
    let map1 = SMap.merge (fun _ u v -> match u, v with None, None -> None | None, Some _ | Some _, None -> assert false | Some u, Some v -> Some (!u, !v)) r1 r2 in
    SMap.fold (fun _ (u, v) cur -> if cur = 0 then value_compare u v else cur) map1 0
  | _ -> assert false
*)

let value_equal (v1 : value) (v2 : value) = v1 = v2
let value_compare (v1 : value) (v2 : value) = compare v1 v2

let value_lt v1 v2 = value_compare v1 v2 < 0
let value_le v1 v2 = value_compare v1 v2 <= 0
let value_gt v1 v2 = value_compare v1 v2 > 0
let value_ge v1 v2 = value_compare v1 v2 >= 0

exception Match_fail

let is_true (v : value) : bool = Obj.magic v

(*
let is_true = function
  | Constructor ("true", _, None) -> true
  | Constructor ("false", _, None) -> false
  | _ -> assert false
*)

let rec lident_name = function
  | Longident.Lident s -> s
  | Longident.Ldot (_, s) -> s
  | Longident.Lapply (l1, l2) -> lident_name l2

(* let unit = Constructor ("()", 0, None) *)
let unit = Obj.repr ()

(*
let set_env env = function
  | Fun (_, _, _, _, ev) | Function (_, ev) -> ev := env
  | _ -> assert false
*)

let set_env (env : env) f =
  let ev = Obj.magic (
      if Obj.tag f = tag_Fun then
        Obj.field f 4
      else if Obj.tag f = tag_Function then
        Obj.field f 1
      else
        assert false
    )
  in
  ev := env

(*
let rec eval_fun_or_function envref expr =
  match expr.pexp_desc with
  | Pexp_function cl -> Function (cl, envref)
  | Pexp_fun (label, default, p, e) -> Fun (label, default, p, e, envref)
  | Pexp_constraint (e, _) | Pexp_coerce (e, _, _) | Pexp_newtype (_, e) ->
    eval_fun_or_function envref e
  | _ -> failwith "unsupported rhs of rec"
*)

let rec eval_fun_or_function (envref : env ref) expr =
  match expr.pexp_desc with
  | Pexp_function cl ->
    let r = Obj.new_block tag_Function 2 in
    Obj.set_field r 0 (Obj.repr cl);
    Obj.set_field r 1 (Obj.repr envref);
    r
  | Pexp_fun (label, default, p, e) ->
    let r = Obj.new_block tag_Fun 5 in
    Obj.set_field r 0 (Obj.repr label);
    Obj.set_field r 1 (Obj.repr default);
    Obj.set_field r 2 (Obj.repr p);
    Obj.set_field r 3 (Obj.repr e);
    Obj.set_field r 4 (Obj.repr envref);
    r
  | Pexp_constraint (e, _) | Pexp_coerce (e, _, _) | Pexp_newtype (_, e) ->
    eval_fun_or_function envref e
  | _ -> failwith "unsupported rhs of rec"

let rec env_get_module env lident =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str env.env_modules)
     with Not_found ->
       if debug then Format.eprintf "Module not found in env: %s@." str; raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env ld in
    (match md with
     | Functor _ -> failwith "Ldot tried to access functor"
     | Module (_, md, _, _) ->
       try SMap.find str md
       with Not_found -> if debug then Format.eprintf "Module not found in submodule: %s@." (String.concat "." (Longident.flatten lident)); raise Not_found)
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_value env lident =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str env.env_vars)
     with Not_found ->
       if debug then Format.eprintf "Variable not found in env: %s@." str; raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env ld in
    (match md with
     | Functor _ -> failwith "Ldot tried to access functor"
     | Module (md, _, _, _) ->
       try SMap.find str md
       with Not_found -> if debug then Format.eprintf "Value not found in submodule: %s@." (String.concat "." (Longident.flatten lident)); raise Not_found)
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_constr env lident =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str env.env_constructors)
     with Not_found ->
       if debug then Format.eprintf "Constructor not found in env: %s@." str; raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env ld in
    (match md with
     | Functor _ -> failwith "Ldot tried to access functor"
     | Module (_, _, md, _) ->
       try SMap.find str md
       with Not_found -> if debug then Format.eprintf "Constructor not found in submodule: %s@." (String.concat "." (Longident.flatten lident)); raise Not_found)
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_field env lident =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str env.env_fields)
     with Not_found ->
       (* if debug then Format.eprintf "Field not found in env: %s@." str; raise Not_found *) (false, 0, SMap.empty))
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env ld in
    (match md with
     | Functor _ -> failwith "Ldot tried to access functor"
     | Module (_, _, _, md) ->
       try SMap.find str md
       with Not_found -> if debug then Format.eprintf "Field not found in submodule: %s@." (String.concat "." (Longident.flatten lident)); raise Not_found)
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_set_value key v env =
  { env with env_vars = SMap.add key (true, v) env.env_vars }

let env_set_module key m env =
  { env with env_modules = SMap.add key (true, m) env.env_modules }

let env_set_constr key c env =
  { env with env_constructors = SMap.add key (true, c) env.env_constructors }

let env_set_field key f env =
  { env with env_fields = SMap.add key (true, f) env.env_fields }

let env_extend exported env (ve1, me1, ce1, fe1) =
  {
    env_vars = SMap.fold (fun key v ve -> SMap.add key (exported, v) ve) ve1 env.env_vars ;
    env_modules = SMap.fold (fun key m me -> SMap.add key (exported, m) me) me1 env.env_modules ;
    env_constructors = SMap.fold (fun key c ce -> SMap.add key (exported, c) ce) ce1 env.env_constructors ;
    env_fields = SMap.fold (fun key f fe -> SMap.add key (exported, f) fe) fe1 env.env_fields ;
  }

let make_module env =
  let ve = SMap.map snd (SMap.filter (fun _ (b, _) -> b) env.env_vars) in
  let me = SMap.map snd (SMap.filter (fun _ (b, _) -> b) env.env_modules) in
  let ce = SMap.map snd (SMap.filter (fun _ (b, _) -> b) env.env_constructors) in
  let fe = SMap.map snd (SMap.filter (fun _ (b, _) -> b) env.env_fields) in
  Module (ve, me, ce, fe)

let prevent_export env =
  {
    env_vars = SMap.map (fun (_, x) -> (false, x)) env.env_vars ;
    env_modules = SMap.map (fun (_, x) -> (false, x)) env.env_modules ;
    env_constructors = SMap.map (fun (_, x) -> (false, x)) env.env_constructors ;
    env_fields = SMap.map (fun (_, x) -> (false, x)) env.env_fields ;
  }

let empty_env = {
  env_vars = SMap.empty ;
  env_modules = SMap.empty ;
  env_constructors = SMap.empty ;
  env_fields = SMap.empty ;
}

let mkprim f (arity : int) =
  let r = Obj.new_block tag_Prim 2 in
  Obj.set_field r 0 (Obj.repr f);
  Obj.set_field r 1 (Obj.repr arity);
  r

(*
let rec seeded_hash_param meaningful total seed = function
  | Int n -> Hashtbl.seeded_hash seed n
  | Int64 n -> Hashtbl.seeded_hash seed n
  | Float f -> Hashtbl.seeded_hash seed f
  | Tuple l -> 0
  | String s -> Hashtbl.seeded_hash seed (Bytes.to_string s)
  | Constructor (c, _, v) -> Hashtbl.seeded_hash seed c
  | Array a -> 0
  | Record r -> 0
  | Fun _ | Function _ | SeqOr | SeqAnd | InChannel _ | OutChannel _ | Prim _ | Lz _ | ModVal _ | Fun_with_extra_args _ -> assert false

let prim1 f unwrap1 wrap = Prim (fun x -> wrap (f (unwrap1 x)))
let prim2 f unwrap1 unwrap2 wrap = Prim (fun x -> prim1 (f (unwrap1 x)) unwrap2 wrap)
let prim3 f unwrap1 unwrap2 unwrap3 wrap = Prim (fun x -> prim2 (f (unwrap1 x)) unwrap2 unwrap3 wrap)
let prim4 f unwrap1 unwrap2 unwrap3 unwrap4 wrap = Prim (fun x -> prim3 (f (unwrap1 x)) unwrap2 unwrap3 unwrap4 wrap)
let prim5 f unwrap1 unwrap2 unwrap3 unwrap4 unwrap5 wrap = Prim (fun x -> prim4 (f (unwrap1 x)) unwrap2 unwrap3 unwrap4 unwrap5 wrap)

let wrap_int n = Int n
let unwrap_int v = match v with Int n -> n | _ -> assert false

let wrap_int64 n = Int64 n
let unwrap_int64 v = match v with Int64 n -> n | _ -> assert false

let wrap_float f = Float f
let unwrap_float v = match v with Float f -> f | _ -> assert false

let unwrap_bool = is_true
let wrap_bool b = if b then Constructor ("true", 1, None) else Constructor ("false", 0, None)

let wrap_unit () = unit
let unwrap_unit = function Constructor ("()", _, None) -> () | _ -> assert false

let wrap_bytes s = String s
let unwrap_bytes = function String s -> s | _ -> assert false
let wrap_string s = String (Bytes.of_string s)
let unwrap_string = function String s -> Bytes.to_string s | _ -> assert false
let wrap_string_unsafe s = String (Bytes.unsafe_of_string s)
let unwrap_string_unsafe = function String s -> Bytes.unsafe_to_string s | _ -> assert false

let wrap_char c = Int (int_of_char c)
let unwrap_char = function Int n -> char_of_int (n land 255) | _ -> assert false

let wrap_array wrapf a = Array (Array.map wrapf a)
let unwrap_array unwrapf = function Array a -> Array.map unwrapf a | _ -> assert false

let wrap_array_id a = Array a
let unwrap_array_id = function Array a -> a | _ -> assert false

let wrap_in_channel ic = InChannel ic
let unwrap_in_channel = function InChannel ic -> ic | _ -> assert false

let wrap_out_channel oc = OutChannel oc
let unwrap_out_channel = function OutChannel oc -> oc | _ -> assert false

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
  | x :: l -> Constructor ("::", 0, Some (Tuple [wrapf x; wrap_list wrapf l]))
let rec unwrap_list unwrapf = function
  | Constructor ("[]", _, None) -> []
  | Constructor ("::", _, Some (Tuple [x; l])) -> unwrapf x :: unwrap_list unwrapf l
  | _ -> assert false

let unwrap_marshal_flag = function
  | Constructor ("No_sharing", _, None) -> Marshal.No_sharing
  | Constructor ("Closures", _, None) -> Marshal.Closures
  | Constructor ("Compat_32", _, None) -> Marshal.Compat_32
  | _ -> assert false
*)

external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash"
external open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"
external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"
external close_desc : int -> unit = "caml_sys_close"
external set_out_channel_name: out_channel -> string -> unit = "caml_ml_set_channel_name"
external out_channels_list : unit -> out_channel list = "caml_ml_out_channels_list"
external unsafe_output : out_channel -> bytes -> int -> int -> unit = "caml_ml_output_bytes"
external unsafe_output_string : out_channel -> string -> int -> int -> unit = "caml_ml_output"
external set_in_channel_name: in_channel -> string -> unit = "caml_ml_set_channel_name"
external unsafe_input : in_channel -> bytes -> int -> int -> int = "caml_ml_input"
external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"
external random_seed : unit -> int array = "caml_sys_random_seed"
external digest_unsafe_string : string -> int -> int -> string = "caml_md5_string"
external marshal_to_channel : out_channel -> 'a -> unit list -> unit = "caml_output_value"
external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external input_scan_line : in_channel -> int = "caml_ml_input_scan_line"

(*
let unwrap_position = function
  | Record r -> Lexing.{
      pos_fname = unwrap_string !(SMap.find "pos_fname" r);
      pos_lnum = unwrap_int !(SMap.find "pos_lnum" r);
      pos_bol = unwrap_int !(SMap.find "pos_bol" r);
      pos_cnum = unwrap_int !(SMap.find "pos_cnum" r);
    }
  | _ -> assert false

let wrap_position pos =
  Lexing.(Record (
      SMap.add "pos_fname" (ref (wrap_string pos.pos_fname)) (
        SMap.add "pos_lnum" (ref (wrap_int pos.pos_lnum)) (
          SMap.add "pos_bol" (ref (wrap_int pos.pos_bol)) (
            SMap.singleton "pos_cnum" (ref (wrap_int pos.pos_cnum))
    )))))

*)
type parser_env =
  { mutable s_stack : int array;        (* States *)
    mutable v_stack : Obj.t array;      (* Semantic attributes *)
    mutable symb_start_stack : Lexing.position array; (* Start positions *)
    mutable symb_end_stack : Lexing.position array;   (* End positions *)
    mutable stacksize : int;            (* Size of the stacks *)
    mutable stackbase : int;            (* Base sp for current parse *)
    mutable curr_char : int;            (* Last token read *)
    mutable lval : Obj.t;               (* Its semantic attribute *)
    mutable symb_start : Lexing.position;      (* Start pos. of the current symbol*)
    mutable symb_end : Lexing.position;        (* End pos. of the current symbol *)
    mutable asp : int;                  (* The stack pointer for attributes *)
    mutable rule_len : int;             (* Number of rhs items in the rule *)
    mutable rule_number : int;          (* Rule number to reduce by *)
    mutable sp : int;                   (* Saved sp for parse_engine *)
    mutable state : int;                (* Saved state for parse_engine *)
    mutable errflag : int }             (* Saved error flag for parse_engine *)

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
    names_block : string }

type parser_input =
    Start
  | Token_read
  | Stacks_grown_1
  | Stacks_grown_2
  | Semantic_action_computed
  | Error_detected

(*
let unwrap_parser_input = function
  | Constructor ("Start", _, None) -> Start
  | Constructor ("Token_read", _, None) -> Token_read
  | Constructor ("Stacks_grown_1", _, None) -> Stacks_grown_1
  | Constructor ("Stacks_grown_2", _, None) -> Stacks_grown_2
  | Constructor ("Semantic_action_computed", _, None) -> Semantic_action_computed
  | Constructor ("Error_detected", _, None) -> Error_detected
  | _ -> assert false
*)

type parser_output =
    Read_token
  | Raise_parse_error
  | Grow_stacks_1
  | Grow_stacks_2
  | Compute_semantic_action
  | Call_error_function

(*
let wrap_parser_output = function
  | Read_token -> cc "Read_token" 0
  | Raise_parse_error -> cc "Raise_parse_error" 1
  | Grow_stacks_1 -> cc "Grow_stacks_1" 2
  | Grow_stacks_2 -> cc "Grow_stacks_2" 3
  | Compute_semantic_action -> cc "Compute_semantic_action" 4
  | Call_error_function -> cc "Call_error_function" 5
*)

let apply_ref = ref (fun _ _ -> assert false)

(*
let unwrap_parser_env = function
  | Record r ->
    {
      s_stack = unwrap_array unwrap_int !(SMap.find "s_stack" r);
      v_stack = Obj.magic (unwrap_array_id !(SMap.find "v_stack" r));
      symb_start_stack = unwrap_array unwrap_position !(SMap.find "symb_start_stack" r);
      symb_end_stack = unwrap_array unwrap_position !(SMap.find "symb_end_stack" r);
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
      errflag = unwrap_int !(SMap.find "errflag" r);
    }
  | _ -> assert false

let sync_parser_env pe = function
  | Record r ->
    let open Parsing in
    SMap.find "s_stack" r := wrap_array wrap_int pe.s_stack;
    SMap.find "v_stack" r := wrap_array_id (Obj.magic pe.v_stack);
    SMap.find "symb_start_stack" r := wrap_array wrap_position pe.symb_start_stack;
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
    SMap.find "errflag" r := wrap_int pe.errflag;
  | _ -> assert false

let unwrap_parse_tables syncenv = function
  | Record r ->
    let actions = unwrap_array (fun fv ->
        fun pe -> sync_parser_env pe syncenv; Obj.repr (!apply_ref fv [(Nolabel, syncenv)])
      ) !(SMap.find "actions" r) in
    {
      actions = actions ;
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
      error_function = (fun s -> unwrap_unit (!apply_ref !(SMap.find "error_function" r) [(Nolabel, wrap_string s)]));
      names_const = unwrap_string_unsafe !(SMap.find "names_const" r);
      names_block = unwrap_string_unsafe !(SMap.find "names_block" r);
    }
  | _ -> assert false
*)

external parse_engine : parse_tables -> parser_env -> parser_input -> Obj.t -> parser_output = "caml_parse_engine"
external lex_engine : Lexing.lex_tables -> int -> Lexing.lexbuf -> int = "caml_lex_engine"
external new_lex_engine : Lexing.lex_tables -> int -> Lexing.lexbuf -> int = "caml_new_lex_engine"

let last_parse_tables = ref (Obj.repr 0)
let last_parse_tables_converted = ref (Obj.repr 0)
let parse_engine_wrapper tables env input token =
  let parse_tables_converted =
    if tables == !last_parse_tables then
      Obj.magic !last_parse_tables_converted
    else begin
      last_parse_tables := tables;
      let tables : parse_tables = Obj.magic tables in
      let cvrt = {
        tables with
        actions = Array.map (fun f -> fun pe -> !apply_ref f [(Nolabel, Obj.repr pe)]) (Obj.magic tables.actions)
      } in
      last_parse_tables_converted := Obj.repr cvrt;
      cvrt
    end
  in
  parse_engine parse_tables_converted env input token

(*
let parse_engine_wrapper tables env input token =
  let nenv = unwrap_parser_env env in
  let tbls = unwrap_parse_tables env tables in
  let obj =
    if input = Semantic_action_computed then Obj.repr token else
    match token with
      | Constructor (c, d, None) ->
        Obj.repr d
      | Constructor (c, d, Some arg) ->
        let w = Obj.repr (Some arg) in
        Obj.set_tag w d;
        w
      | _ -> assert false
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
      lex_curr_p = unwrap_position !(SMap.find "lex_curr_p" r);
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
    SMap.find "lex_curr_p" r := wrap_position lb.lex_curr_p;
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
      lex_code = gs "lex_code";
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
*)

let id x = x

(*
let parse_engine_prim = prim4 parse_engine_wrapper id id unwrap_parser_input id wrap_parser_output
let lex_engine_prim = prim3 lex_engine_wrapper id unwrap_int id wrap_int
let new_lex_engine_prim = prim3 new_lex_engine_wrapper id unwrap_int id wrap_int
*)

let initial_env = ref (empty_env : env)
let exn_id = ref 0
let declare_builtin_constructor name d arity =
  initial_env := env_set_constr name (d, CTuple arity, false) !initial_env
let declare_exn name arity =
  let d = !exn_id in
  incr exn_id;
  initial_env := env_set_constr name (d, CTuple arity, true) !initial_env;
  d

let not_found_exn_id = declare_exn "Not_found" 0
let not_found_exn =
  let r = Obj.new_block 0 1 in
  Obj.set_field r 0 (Obj.repr not_found_exn_id);
  r
let _ = declare_exn "Exit" 0
let _ = declare_exn "Invalid_argument" 1
let _ = declare_exn "Failure" 1
let _ = declare_exn "Match_failure" 1
let assert_failure_id = declare_exn "Assert_failure" 1
let _ = declare_exn "Sys_blocked_io" 0
let _ = declare_exn "Sys_error" 1
let _ = declare_exn "End_of_file" 0
let _ = declare_exn "Division_by_zero" 0
let _ = declare_exn "Undefined_recursive_module" 1

let _ = declare_builtin_constructor "false" 0 0
let _ = declare_builtin_constructor "true" 1 0
let _ = declare_builtin_constructor "None" 0 0
let _ = declare_builtin_constructor "Some" 0 1
let _ = declare_builtin_constructor "[]" 0 0
let _ = declare_builtin_constructor "::" 0 2
let _ = declare_builtin_constructor "()" 0 0

external caml_register_named_value : string -> Obj.t -> unit = "caml_register_named_value"
external caml_ml_set_channel_name : Obj.t -> string -> unit = "caml_ml_set_channel_name"
external caml_ml_close_channel : Obj.t -> unit = "caml_ml_close_channel"

let eval_expr_fun = ref (fun x y -> assert false)

let prims = [
  ("%apply", mkprim (fun vf v -> !apply_ref vf [(Nolabel, v)]) 2);
  ("%revapply", mkprim (fun v vf -> !apply_ref vf [(Nolabel, v)]) 2);
  ("%raise", mkprim (fun v -> raise (InternalException v)) 1);
  ("%reraise", mkprim (fun v -> raise (InternalException v)) 1);
  ("%raise_notrace", mkprim (fun v -> raise (InternalException v)) 1);
  ("%sequand", (let r = Obj.new_block tag_SeqAnd 1 in Obj.set_field r 0 (Obj.repr 0); r));
  ("%sequor", (let r = Obj.new_block tag_SeqOr 1 in Obj.set_field r 0 (Obj.repr 0); r));
  ("%boolnot", mkprim not 1);
  ("%negint", mkprim ( ~- ) 1);
  ("%succint", mkprim succ 1);
  ("%predint", mkprim pred 1);
  ("%addint", mkprim ( + ) 2);
  ("%subint", mkprim ( - ) 2);
  ("%mulint", mkprim ( * ) 2);
  ("%divint", mkprim ( / ) 2);
  ("%modint", mkprim ( mod ) 2);
  ("%andint", mkprim ( land ) 2);
  ("%orint", mkprim ( lor ) 2);
  ("%xorint", mkprim ( lxor ) 2);
  ("%lslint", mkprim ( lsl ) 2);
  ("%lsrint", mkprim ( lsr ) 2);
  ("%asrint", mkprim ( asr ) 2);
  ("%addfloat", mkprim ( +. ) 2);
  ("%subfloat", mkprim ( -. ) 2);
  ("%mulfloat", mkprim ( *. ) 2);
  ("%divfloat", mkprim ( /. ) 2);
  ("%floatofint", mkprim float_of_int 1);
  ("%intoffloat", mkprim int_of_float 1);
  ("caml_float_of_string", mkprim float_of_string 1);
  ("%lessthan", mkprim value_lt 2);
  ("%lessequal", mkprim value_le 2);
  ("%greaterthan", mkprim value_gt 2);
  ("%greaterequal", mkprim value_ge 2);
  ("%compare", mkprim value_compare 2);
  ("%equal", mkprim value_equal 2);
  ("%notequal", mkprim (fun x y -> not (value_equal x y)) 2);
  ("%eq", mkprim ( == ) 2);
  ("%noteq", mkprim ( != ) 2);
  ("%identity", mkprim (fun x -> x) 1);
  ("caml_register_named_value", mkprim caml_register_named_value 2);
  ("caml_int64_float_of_bits", mkprim Int64.float_of_bits 1);
  ("caml_ml_open_descriptor_out", mkprim open_descriptor_out 1);
  ("caml_ml_open_descriptor_in", mkprim open_descriptor_in 1);
  ("caml_sys_open", mkprim open_desc 3);
  ("caml_sys_close", mkprim close_desc 1);
  ("caml_ml_set_channel_name", mkprim caml_ml_set_channel_name 2);
  ("caml_ml_close_channel", mkprim caml_ml_close_channel 1);
  ("caml_ml_out_channels_list", mkprim out_channels_list 1);
  ("caml_ml_output_bytes", mkprim unsafe_output 4);
  ("caml_ml_output", mkprim unsafe_output_string 4);
  ("caml_ml_output_int", mkprim output_binary_int 2);
  ("caml_ml_output_char", mkprim output_char 2);
  ("caml_ml_flush", mkprim flush 1);
  ("caml_ml_input_char", mkprim input_char 1);
  ("caml_ml_input_int", mkprim input_binary_int 1);
  ("caml_ml_input_scan_line", mkprim input_scan_line 1);
  ("caml_ml_input", mkprim unsafe_input 4);
  ("caml_ml_seek_in", mkprim seek_in 2);
  ("caml_ml_pos_out", mkprim pos_out 1);
  ("caml_ml_pos_in", mkprim pos_in 1);
  ("caml_ml_seek_out", mkprim seek_out 2);
  ("%makemutable", mkprim ref 1);
  ("%field0", mkprim fst 1);
  ("%field1", mkprim snd 1);
  ("%setfield0", mkprim ( := ) 2);
  ("%incr", mkprim incr 1);
  ("%decr", mkprim decr 1);
  ("%ignore", mkprim (fun _ -> ()) 1);
  ("caml_format_int", mkprim format_int 2);
  ("caml_format_float", mkprim format_float 2);
  ("caml_int_of_string", mkprim int_of_string 1);
  ("caml_output_value", mkprim marshal_to_channel 3);
  ("caml_output_value_to_buffer", mkprim Marshal.to_buffer 5);
  ("caml_input_value", mkprim input_value 1);
  ("caml_sys_exit", mkprim exit 1);
  ("caml_parse_engine", mkprim parse_engine_wrapper 4);
  ("caml_lex_engine", mkprim lex_engine 3);
  ("caml_new_lex_engine", mkprim new_lex_engine 3);

  (* Sys *)
  ("caml_sys_get_argv", mkprim (fun _ -> ("", Sys.argv)) 1);
  ("caml_sys_get_config", mkprim (fun _ -> ("Unix", 0, true)) 1);
  ("%big_endian", mkprim (fun _ -> Sys.big_endian) 1);
  ("%word_size", mkprim (fun _ -> 64) 1);
  ("%int_size", mkprim (fun _ -> 64) 1);
  ("%max_wosize", mkprim (fun _ -> 1000000) 1);
  ("%ostype_unix", mkprim (fun _ -> false) 1);
  ("%ostype_win32", mkprim (fun _ -> false) 1);
  ("%ostype_cygwin", mkprim (fun _ -> false) 1);
  ("%backend_type", mkprim (fun _ -> Sys.Other "Interpreter") 1);
  ("caml_sys_getenv", mkprim (fun _ -> raise (InternalException not_found_exn)) 1);
  ("caml_sys_file_exists", mkprim Sys.file_exists 1);
  ("caml_sys_getcwd", mkprim Sys.getcwd 1);
  ("caml_sys_rename", mkprim Sys.rename 2);
  ("caml_sys_remove", mkprim Sys.remove 1);
  ("caml_sys_system_command", mkprim (fun x -> Format.printf "%s@." x; Sys.command x) 1);

  (* Bytes *)
  ("caml_create_bytes", mkprim Bytes.create 1);
  ("caml_fill_bytes", mkprim Bytes.unsafe_fill 4);
  ("%bytes_to_string", mkprim (fun v -> v) 1);
  ("%bytes_of_string", mkprim (fun v -> v) 1);
  ("%string_length", mkprim Bytes.length 1);
  ("%bytes_length", mkprim Bytes.length 1);
  ("%string_safe_get", mkprim Bytes.get 2);
  ("%string_unsafe_get", mkprim Bytes.unsafe_get 2);
  ("%bytes_safe_get", mkprim Bytes.get 2);
  ("%bytes_unsafe_get", mkprim Bytes.unsafe_get 2);
  ("%bytes_safe_set", mkprim Bytes.set 3);
  ("%bytes_unsafe_set", mkprim Bytes.unsafe_set 3);
  ("caml_blit_string", mkprim String.blit 5);
  ("caml_blit_bytes", mkprim Bytes.blit 5);

  (* Lazy *)
  ("%lazy_force", mkprim (fun v ->
       if Obj.tag v = tag_Lz_computed then
         Obj.field v 0
       else begin
         assert (Obj.tag v = tag_Lz);
         let r = !eval_expr_fun (Obj.magic (Obj.field v 0)) (Obj.magic (Obj.field v 1)) in
         Obj.set_tag v tag_Lz_computed;
         Obj.set_field v 0 r;
         r
       end
     ) 1);

  (* Int64 *)
  ("%int64_neg", mkprim Int64.neg 1);
  ("%int64_add", mkprim Int64.add 2);
  ("%int64_sub", mkprim Int64.sub 2);
  ("%int64_mul", mkprim Int64.mul 2);
  ("%int64_div", mkprim Int64.div 2);
  ("%int64_mod", mkprim Int64.rem 2);
  ("%int64_and", mkprim Int64.logand 2);
  ("%int64_or", mkprim Int64.logor 2);
  ("%int64_xor", mkprim Int64.logxor 2);
  ("%int64_lsl", mkprim Int64.shift_left 2);
  ("%int64_lsr", mkprim Int64.shift_right_logical 2);
  ("%int64_asr", mkprim Int64.shift_right 2);
  ("%int64_of_int", mkprim Int64.of_int 1);
  ("%int64_to_int", mkprim Int64.to_int 1);
  ("caml_int64_of_string", mkprim Int64.of_string 1);

  (* Int32 *)
  ("caml_int32_of_string", mkprim Int32.of_string 1);
  ("%int32_neg", mkprim Int32.neg 1);

  (* Nativeint *)
  ("%nativeint_neg", mkprim Nativeint.neg 1);
  ("%nativeint_add", mkprim Nativeint.add 2);
  ("%nativeint_sub", mkprim Nativeint.sub 2);
  ("%nativeint_mul", mkprim Nativeint.mul 2);
  ("%nativeint_div", mkprim Nativeint.div 2);
  ("%nativeint_mod", mkprim Nativeint.rem 2);
  ("%nativeint_and", mkprim Nativeint.logand 2);
  ("%nativeint_or", mkprim Nativeint.logor 2);
  ("%nativeint_xor", mkprim Nativeint.logxor 2);
  ("%nativeint_lsl", mkprim Nativeint.shift_left 2);
  ("%nativeint_lsr", mkprim Nativeint.shift_right_logical 2);
  ("%nativeint_asr", mkprim Nativeint.shift_right 2);
  ("%nativeint_of_int", mkprim Nativeint.of_int 1);
  ("%nativeint_to_int", mkprim Nativeint.to_int 1);
  ("caml_nativeint_of_string", mkprim Nativeint.of_string 1);

  (* Array *)
  ("caml_make_vect", mkprim Array.make 2);
  ("%array_length", mkprim Array.length 1);
  ("caml_array_sub", mkprim Array.sub 3);
  ("%array_safe_get", mkprim Array.get 2);
  ("%array_unsafe_get", mkprim Array.unsafe_get 2);
  ("%array_safe_set", mkprim Array.set 3);
  ("%array_unsafe_set", mkprim Array.unsafe_set 3);
  ("caml_array_blit", mkprim Array.blit 5);
  ("caml_array_append", mkprim append_prim 2);

  (* Hashtbl *)
  ("caml_hash", mkprim seeded_hash_param 4);

  (* Weak *)
  ("caml_weak_create", mkprim Weak.create 1);
  ("caml_weak_get", mkprim Weak.get 2);
  ("caml_weak_get_copy", mkprim Weak.get_copy 2);
  ("caml_weak_set", mkprim Weak.set 3);
  ("caml_weak_check", mkprim Weak.check 2);
  ("caml_weak_blit", mkprim Weak.blit 5);

  (* Random *)
  ("caml_sys_random_seed", mkprim random_seed 1);

  (* Digest *)
  ("caml_md5_string", mkprim digest_unsafe_string 3);
  ("caml_md5_chan", mkprim Digest.channel 2);

  (* Ugly *)
  ("%obj_size", mkprim Obj.size 1);
  ("caml_obj_block", mkprim Obj.new_block 2);
  ("caml_obj_tag", mkprim Obj.tag 1);
  ("%obj_is_int", mkprim Obj.is_int 1);
  ("%obj_field", mkprim Obj.field 2);
  ("%obj_set_field", mkprim Obj.set_field 3);
]

let prims = List.fold_left (fun env (name, v) -> SMap.add name v env) SMap.empty prims
let hash_variant_name (name : string) = (Hashtbl.hash name) land (1 lsl 30 - 1)

let static_records = [
  "parse_tables"; "parser_env"; "lex_tables"; "lexbuf"; "position"; "ref"; "compilation_unit"; "library"
]

(*
let rec expr_label_shape = function
  | Pexp_fun (label, default, _, e) -> (label, default) :: expr_label_shape e.pexp_desc
  | Pexp_function _ -> [(Nolabel, None)]
  | _ -> []

let fun_label_shape = function
  | Fun (lab, default, _, e, _) -> (lab, default) :: expr_label_shape e.pexp_desc
  | Function _ -> [(Nolabel, None)]
  | Prim _ -> [(Nolabel, None)]
  | SeqOr | SeqAnd -> [(Nolabel, None); (Nolabel, None)]
  | _ -> []
*)

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

let fmt_ebb_of_string_fct = ref (Obj.repr 0)

let mkblock tag l =
  let r = Obj.new_block tag (List.length l) in
  List.iteri (fun i x -> Obj.set_field r i x) l;
  r

let rec obj_copy obj1 obj2 i j =
  if i = j then ()
  else (Obj.set_field obj2 i (Obj.field obj1 i); obj_copy obj1 obj2 (i + 1) j)

let rec find_field fnames idx name =
  match fnames with
  | [] -> assert false
  | fname :: fnames -> if fname = name then idx else find_field fnames (idx + 1) name

let rec apply vf args =
  let vf, extral, extram =
    if Obj.tag vf = tag_Fun_with_extra_args then
      (Obj.field vf 0, Obj.magic (Obj.field vf 1), Obj.magic (Obj.field vf 2))
    else
      (vf, [], SMap.empty)
  in
  assert (extral = []);
  let apply_labelled vf (lab, arg) =
    assert (Obj.tag vf = tag_Fun);
    let (label : arg_label) = Obj.magic (Obj.field vf 0) in
    let (default : expression option) = Obj.magic (Obj.field vf 1) in
    let (p : pattern) = Obj.magic (Obj.field vf 2) in
    let (e : expression) = Obj.magic (Obj.field vf 3) in
    let (fenv : env ref) = Obj.magic (Obj.field vf 4) in
(*    match label, lab, default with
    | Optional s, Labelled s', None ->
      assert (s = s');
      eval_expr (pattern_bind !fenv p (Constructor ("Some", 0, Some arg))) e
    | Optional s, Labelled s', Some _ | Optional s, Optional s', None | Labelled s, Labelled s', None ->
      assert (s = s');
      eval_expr (pattern_bind !fenv p arg) e
    | Optional s, Optional s', Some def ->
      assert (s = s');
      let arg = match arg with
        | Constructor ("None", 0, None) -> eval_expr !fenv def
        | Constructor ("Some", 0, Some arg) -> arg
        | _ -> assert false
      in eval_expr (pattern_bind !fenv p arg) e *)
    match label with
    | Nolabel -> assert false
    | Labelled s ->
      assert (lab = Labelled s); assert (default = None);
      eval_expr (pattern_bind !fenv p arg) e
    | Optional s ->
      match lab with
      | Nolabel -> assert false
      | Labelled s' ->
        assert (s = s');
        let arg = match default with
          | None -> Obj.repr (Some arg)
          | Some _ -> arg
        in eval_expr (pattern_bind !fenv p arg) e
      | Optional s' ->
        assert (s = s');
        let arg = match default with
          | None -> arg
          | Some def -> match Obj.magic arg with None -> eval_expr !fenv def | Some arg -> arg
        in
        eval_expr (pattern_bind !fenv p arg) e
  in
  let apply_optional_noarg vf =
    assert (Obj.tag vf = tag_Fun);
    let (label : arg_label) = Obj.magic (Obj.field vf 0) in
    let (default : expression option) = Obj.magic (Obj.field vf 1) in
    let (p : pattern) = Obj.magic (Obj.field vf 2) in
    let (e : expression) = Obj.magic (Obj.field vf 3) in
    let (fenv : env ref) = Obj.magic (Obj.field vf 4) in
    assert (match label with Optional _ -> true | _ -> false);
    let arg = match default with
      | None -> Obj.repr None
      | Some def -> eval_expr !fenv def
    in
    eval_expr (pattern_bind !fenv p arg) e
  in
  let unlabelled = List.map snd (List.filter (fun (lab, _) -> lab = Nolabel) args) in
  let with_label = ref (List.fold_left (fun wl (lab, arg) ->
      match lab with Nolabel -> wl | Optional s | Labelled s -> SMap.add s (lab, arg) wl
    ) extram args)
  in
  let has_labelled = not (SMap.is_empty !with_label) in
  let rec apply_one vf arg =
    let tag = Obj.tag vf in
    if tag = tag_Fun then
      let (lab : arg_label) = Obj.magic (Obj.field vf 0) in
      let (p : pattern) = Obj.magic (Obj.field vf 2) in
      let (e : expression) = Obj.magic (Obj.field vf 3) in
      let (fenv : env ref) = Obj.magic (Obj.field vf 4) in
      match lab with
      | Nolabel -> eval_expr (pattern_bind !fenv p arg) e
      | Labelled s ->
        if has_labelled then begin
          assert (SMap.mem s !with_label);
          let v = SMap.find s !with_label in
          with_label := SMap.remove s !with_label;
          apply_one (apply_labelled vf v) arg
        end else
          eval_expr (pattern_bind !fenv p arg) e
      | Optional s ->
        if has_labelled && SMap.mem s !with_label then begin
          let v = SMap.find s !with_label in
          with_label := SMap.remove s !with_label;
          apply_one (apply_labelled vf v) arg
        end else
          apply_one (apply_optional_noarg vf) arg
    else if tag = tag_Function then
      let cl = Obj.magic (Obj.field vf 0) in
      let fenv = Obj.magic (Obj.field vf 1) in
      eval_match !fenv cl (Ok arg)
    else if tag = tag_Prim then
      let (arity : int) = Obj.magic (Obj.field vf 1) in
      let current_args = Obj.size vf - 2 in
      if current_args + 1 = arity then
        let rec apply_loop f clos i j =
          if i = j then f
          else apply_loop (Obj.magic f (Obj.field clos i)) clos (i + 1) j
        in
        Obj.magic (apply_loop (Obj.field vf 0) vf 2 (Obj.size vf)) arg
      else
        let no = Obj.new_block tag_Prim (Obj.size vf + 1) in
        obj_copy vf no 0 (Obj.size vf);
        Obj.set_field no (Obj.size vf) arg;
        no
    else if tag = tag_SeqOr then
      if is_true arg then mkprim (fun _ -> true) 1 else mkprim id 1
    else if tag = tag_SeqAnd then
      if is_true arg then mkprim id 1 else mkprim (fun _ -> false) 1
    else
      assert false
        (*
    match vf with
    | Fun (Nolabel, default, p, e, fenv) -> eval_expr (pattern_bind !fenv p arg) e
    | Fun ((Labelled s | Optional s) as lab, default, p, e, fenv) ->
      if has_labelled then
        if SMap.mem s !with_label then begin
          let v = SMap.find s !with_label in
          with_label := SMap.remove s !with_label;
          apply_one (apply_labelled vf v) arg
        end else begin
          assert (lab = Optional s);
          apply_one (apply_optional_noarg vf) arg
        end
      else if lab = Optional s then
        apply_one (apply_optional_noarg vf) arg
      else
        eval_expr (pattern_bind !fenv p arg) e
    | Function (cl, fenv) -> eval_match !fenv cl (Ok arg)
    | Prim prim -> prim arg
    | SeqOr -> if is_true arg then mkprim (fun _ -> true) 1 else mkprim id 1
    | SeqAnd -> if is_true arg then mkprim id 1 else mkprim (fun _ -> false) 1
    | v -> (*Format.eprintf "%a@." pp_print_value v; *)assert false
           *)
  in
  if SMap.is_empty !with_label then (* Special case to get tail recursion *)
    List.fold_left apply_one vf unlabelled
  else
    let vf = List.fold_left apply_one vf unlabelled in
    let rec apply_loop vf =
      if SMap.is_empty !with_label then
        vf
      else if Obj.tag vf = tag_Fun && Obj.magic (Obj.field vf 0) <> Nolabel then
        let (lab : arg_label) = Obj.magic (Obj.field vf 0) in
        let s = match lab with Nolabel -> assert false | Labelled s -> s | Optional s -> s in
        if SMap.mem s !with_label then begin
          let v = SMap.find s !with_label in
          with_label := SMap.remove s !with_label;
          apply_loop (apply_labelled vf v)
        end else begin
          assert (match lab with Optional _ -> true | _ -> false);
          apply_loop (apply_optional_noarg vf)
        end
      else
          let r = Obj.new_block tag_Fun_with_extra_args 3 in
          Obj.set_field r 0 vf;
          Obj.set_field r 1 (Obj.repr []);
          Obj.set_field r 2 (Obj.repr !with_label);
          r
    in
    apply_loop vf

and eval_expr env expr =
  match expr.pexp_desc with
  | Pexp_ident { txt = lident } -> env_get_value env lident
  | Pexp_constant c -> value_of_constant c
  | Pexp_let (f, vals, e) ->
    if f = Nonrecursive then
      let nenv = List.fold_left (bind_value env) env vals in
      eval_expr nenv e
    else
      let er = ref env in
      let nenv = List.fold_left (bind_value_rec er) env vals in
      er := nenv; eval_expr nenv e
  | Pexp_function cl ->
    let r = Obj.new_block tag_Function 2 in
    Obj.set_field r 0 (Obj.repr cl);
    Obj.set_field r 1 (Obj.repr (ref env));
    r
  | Pexp_fun (label, default, p, e) ->
    let r = Obj.new_block tag_Fun 5 in
    Obj.set_field r 0 (Obj.repr label);
    Obj.set_field r 1 (Obj.repr default);
    Obj.set_field r 2 (Obj.repr p);
    Obj.set_field r 3 (Obj.repr e);
    Obj.set_field r 4 (Obj.repr (ref env));
    r
  | Pexp_apply (f, l) ->
    let fc = eval_expr env f in
    if Obj.tag fc = tag_SeqOr && List.length l = 2 then
      let arg1 = snd (List.hd l) in
      let arg2 = snd (List.hd (List.tl l)) in
      if is_true (eval_expr env arg1) then Obj.repr true else eval_expr env arg2
    else if Obj.tag fc = tag_SeqAnd && List.length l = 2 then
      let arg1 = snd (List.hd l) in
      let arg2 = snd (List.hd (List.tl l)) in
      if is_true (eval_expr env arg1) then eval_expr env arg2 else Obj.repr false
    else begin
      let args = List.map (fun (lab, e) -> (lab, eval_expr env e)) l in
      if trace then begin match f.pexp_desc with Pexp_ident lident ->
        Format.eprintf "apply %s@." (String.concat "." (Longident.flatten lident.txt));
        incr tracecur;
        (*if !tracecur > tracearg_from then Format.eprintf " %a" (Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff " ") (fun ff (_, v) -> Format.fprintf ff "%a" pp_print_value v)) args; *)
        Format.eprintf "@." | _ -> ()
      end;
      apply fc args
    end
  | Pexp_tuple l ->
    let args = List.map (eval_expr env) l in
    mkblock 0 args
  | Pexp_match (e, cl) -> eval_match env cl (eval_expr_exn env e)
  | Pexp_coerce (e, _, _) -> eval_expr env e
  | Pexp_constraint (e, _) -> eval_expr env e
  | Pexp_sequence (e1, e2) -> let _ = eval_expr env e1 in eval_expr env e2
  | Pexp_while (e1, e2) -> while is_true (eval_expr env e1) do ignore (eval_expr env e2) done; unit
  | Pexp_for (p, e1, e2, flag, e3) ->
    let (v1 : int) = Obj.magic (eval_expr env e1) in
    let (v2 : int) = Obj.magic (eval_expr env e2) in
    if flag = Upto then
      for x = v1 to v2 do
        ignore (eval_expr (pattern_bind env p (Obj.repr x)) e3)
      done
    else
      for x = v1 downto v2 do
        ignore (eval_expr (pattern_bind env p (Obj.repr x)) e3)
      done;
    unit
  | Pexp_ifthenelse (e1, e2, e3) ->
    if is_true (eval_expr env e1) then eval_expr env e2 else (match e3 with None -> unit | Some e3 -> eval_expr env e3)
  | Pexp_unreachable -> failwith "reached unreachable"
  | Pexp_try (e, cs) ->
    (try eval_expr env e with
       InternalException v ->
       try eval_match env cs (Ok v) with Match_fail -> raise (InternalException v)
    )
  | Pexp_construct ({ txt = c }, e) ->
    let d, cdesc, is_exn = env_get_constr env c in
    (match e with
     | None ->
       assert (cdesc = CTuple 0);
       if is_exn then begin
         let r = Obj.new_block 0 1 in
         Obj.set_field r 0 (Obj.repr d);
         r
       end else Obj.repr d
     | Some e ->
       assert (cdesc <> CTuple 0);
       let vs =
         match cdesc with
         | CTuple arity ->
           if arity > 1 then
             match e.pexp_desc with
             | Pexp_tuple l -> List.map (eval_expr env) l
             | _ -> assert false
           else
             [eval_expr env e]
         | CRecord (fields, _) ->
           match e.pexp_desc with
           | Pexp_record (r, e) ->
             assert (e = None);
             assert (List.length r = List.length fields);
             List.map (fun x -> eval_expr env (snd (List.find (fun ({ txt = lident }, _) -> lident_name lident = x) r))) fields @ [Obj.repr fields]
           | _ -> assert false
       in
       if is_exn then
         mkblock 0 (Obj.repr d :: vs)
       else
         mkblock d vs
    )
  | Pexp_variant (cn, e) ->
    let id = Obj.repr (hash_variant_name cn) in
    (match e with
     | None -> let r = Obj.new_block 0 1 in Obj.set_field r 0 id; r
     | Some e -> let r = Obj.new_block 0 2 in Obj.set_field r 0 id; Obj.set_field r 1 (eval_expr env e); r
    )
  | Pexp_record (r, e) ->
    let is_static, _, fds = env_get_field env (fst (List.hd r)).txt in
    let base, fnames = match e with
      | None ->
        let r1 = Obj.new_block 0 ((List.length r) + if is_static then 0 else 1) in
        let fnames = List.map (fun ({ txt = lident }, _) -> lident_name lident) r in
        if not is_static then Obj.set_field r1 (List.length r) (Obj.repr fnames);
        r1, fnames
      | Some e ->
        let r = eval_expr env e in
        let r1 = Obj.new_block 0 (Obj.size r) in
        obj_copy r r1 0 (Obj.size r);
        r1, if is_static then [] else Obj.magic (Obj.field r (Obj.size r - 1))
    in
    let get_field lident =
      if is_static then
        match lident with
        | Longident.Lident n -> SMap.find n fds
        | _ -> let (_, id, _) = env_get_field env lident in id
      else
        find_field fnames 0 (lident_name lident)
    in
    List.fold_left (fun rc ({ txt = lident }, ee) ->
        (*        if lident_name lident = "cd_args" then Format.printf "%d@." (get_field lident); *)
      Obj.set_field rc (get_field lident) (eval_expr env ee); rc
    ) base r
  | Pexp_field (e, { txt = lident }) ->
    let is_static, fieldid, _ = env_get_field env lident in
    let r = eval_expr env e in
    let fieldid =
      if is_static then fieldid else
        let fnames = Obj.magic (Obj.field r (Obj.size r - 1)) in
        find_field fnames 0 (lident_name lident)
    in
    Obj.field r fieldid
  | Pexp_setfield (e1, { txt = lident }, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let is_static, fieldid, _ = env_get_field env lident in
    let fieldid =
      if is_static then fieldid else
        let fnames = Obj.magic (Obj.field v1 (Obj.size v1 - 1)) in
        find_field fnames 0 (lident_name lident)
    in
    Obj.set_field v1 fieldid v2;
    unit
  | Pexp_array l -> Obj.repr (Array.of_list (List.map (eval_expr env) l))
  | Pexp_send _ -> assert false
  | Pexp_new _ -> assert false
  | Pexp_setinstvar _ -> assert false
  | Pexp_override _ -> assert false
  | Pexp_letexception ({ pext_name = { txt = name } ; pext_kind = k }, e) ->
    let nenv =
      match k with
      | Pext_decl (_, typearg) ->
        let arity = match typearg with None -> 0 | Some _ -> 1 in
        let d = !exn_id in incr exn_id; env_set_constr name (d, CTuple arity, true) env
      | Pext_rebind { txt = path } -> env_set_constr name (env_get_constr env path) env
    in
    eval_expr nenv e
  | Pexp_letmodule ({ txt = name }, me, e) ->
    let m = eval_module_expr env me in
    eval_expr (env_set_module name m env) e
  | Pexp_assert e ->
    if is_true (eval_expr env e) then unit else (*failwith "assert failure"*)
      raise (InternalException (mkblock 0 [Obj.repr assert_failure_id; mkblock 0 [Obj.repr ""; Obj.repr 0; Obj.repr 0]]))
  | Pexp_lazy e ->
    let b = Obj.new_block tag_Lz 2 in
    Obj.set_field b 0 (Obj.repr env);
    Obj.set_field b 1 (Obj.repr e);
    b
  | Pexp_poly _ -> assert false
  | Pexp_newtype (_, e) -> eval_expr env e
  | Pexp_open (_, { txt = lident }, e) ->
    let nenv = (match env_get_module env lident with
        | Module (venv, menv, cenv, fenv) -> env_extend false env (venv, menv, cenv, fenv)
        | Functor _ -> assert false
        | exception Not_found -> env (* Module might be a .mli only *))
    in eval_expr nenv e
  | Pexp_object _ -> assert false
  | Pexp_pack me ->
    let mdl = eval_module_expr env me in
    let r = Obj.new_block tag_ModVal 1 in
    Obj.set_field r 0 (Obj.repr mdl);
    r
  | Pexp_extension _ -> assert false

and eval_expr_exn env expr =
  try Ok (eval_expr env expr) with InternalException v -> Error v

and bind_value evalenv bindenv vb =
  let v = eval_expr evalenv vb.pvb_expr in
  pattern_bind bindenv vb.pvb_pat v

and bind_value_rec evalenvref bindenv vb =
  let v = eval_fun_or_function evalenvref vb.pvb_expr in
  pattern_bind bindenv vb.pvb_pat v

(* Returns the environment resulting of matching [v] with [pat] in the environment [env].
   Raises [Match_fail] in case of matching failure.
*)
and pattern_bind env pat v =
  match pat.ppat_desc with
  | Ppat_any -> env
  | Ppat_var { txt = s } -> env_set_value s v env
  | Ppat_alias (p, { txt = s }) ->
    env_set_value s v (pattern_bind env p v)
  | Ppat_constant c ->
    if value_equal (value_of_constant c) v then env else raise Match_fail
  | Ppat_interval (c1, c2) ->
    if value_le (value_of_constant c1) v && value_le v (value_of_constant c2) then env else raise Match_fail
  | Ppat_tuple l ->
    assert (Obj.size v = List.length l);
    pattern_bind_list env l v 0
  | Ppat_construct ({ txt = c }, p) ->
    let d, cdesc, is_exn = env_get_constr env c in
    if cdesc = CTuple 0 && not is_exn then begin
      if (Obj.magic v) = d then env else raise Match_fail
    end else if Obj.tag v = Obj.string_tag then begin
      (* Trying to match a string to a constructor.
         Since we completely ignore typing, there is a case when this can happen,
         if the string is used as a format.
         We recognize this case, and use the fmt_ebb_of_string function
         (defined in the standard library) to parse the string as a format string
         and return the corresponding value.

         This is of course a hack - but no less than the typing hack that is already in
         OCaml's typer.
         However, one drawback is that the same format string might get parsed several times
         at runtime; since we are not pursuing efficiency, and string formatting is linear in
         the size of the string anyway, it is not a real problem.
      *)
      assert (lident_name c = "Format" && d = 0 && cdesc = CTuple 2 && not is_exn);
      let p = match p with None -> assert false | Some p -> p in
      let pl = match p.ppat_desc with Ppat_tuple l -> l | _ -> assert false in
      assert (List.length pl = 2);
      let p1, p2 = List.hd pl, List.hd (List.tl pl) in
      let fmt = apply !fmt_ebb_of_string_fct [(Nolabel, v)] in
      assert (Obj.size fmt = 1);
      let fmt = Obj.field fmt 0 in
      pattern_bind (pattern_bind env p1 fmt) p2 v
    end else begin
      let initfield =
        if is_exn then begin
          assert (Obj.tag v = 0);
          if Obj.magic (Obj.field v 0) <> d then raise Match_fail;
          1
        end else begin
          if Obj.tag v <> d then raise Match_fail;
          0
        end
      in
      match cdesc with
      | CTuple arity ->
        let pats = match p with
          | None -> assert (arity = 0); []
          | Some p ->
            assert (arity > 0);
            if arity > 1 then
              match p.ppat_desc with
              | Ppat_tuple l -> l
              | Ppat_any -> []
              | _ -> assert false
            else
              [p]
        in
        pattern_bind_list env pats v initfield
      | CRecord (fields, fieldids) ->
        match p with
        | None -> assert false
        | Some p -> (*match p.ppat_desc with
          | Ppat_record (rp, _) ->
            List.fold_left (fun env ({ txt = lident }, p) ->
                pattern_bind env p (Obj.field v (SMap.find (lident_name lident) fieldids))
              ) env rp
          | _ ->
            (* Hack: locally expose the field definitions *)
            let (_, env) = List.fold_left (fun (i, env) f ->
              (i + 1, env_set_field f (i, fieldids) env)
            ) (0, env) fields
            in
                      pattern_bind env p v *)
          pattern_bind env p v
    end
    (*
      match v with
      | Constructor (ccn, ddn, e) ->
        if cn <> ccn then raise Match_fail;
        if dn <> ddn then raise Match_fail;
        (match (p, e) with
         | None, None -> env
         | Some p, Some e -> pattern_bind env p e
         | _ -> assert false)
      | String s ->
        assert (lident_name c = "Format");
        let p = match p with None -> assert false | Some p -> p in
        let fmt = apply !fmt_ebb_of_string_fct [(Nolabel, String s)] in
        let fmt = match fmt with | Constructor ("Fmt_EBB", _, Some fmt) -> fmt | _ -> assert false in
        pattern_bind env p (Tuple [fmt; v])
      | _ -> Format.eprintf "cn = %s@.v = %a@." cn pp_print_value v; assert false
    *)
  | Ppat_variant (name, p) ->
    let id = hash_variant_name name in
    assert (Obj.tag v = 0);
    if Obj.magic (Obj.field v 0) <> id then raise Match_fail;
    (match p with
     | None ->
       assert (Obj.size v = 1); env
     | Some p ->
       assert (Obj.size v = 2);
       pattern_bind env p (Obj.field v 1)
    )
  | Ppat_record (rp, _) ->
    let is_static, _, fds = env_get_field env (fst (List.hd rp)).txt in
    let fnames = if is_static then [] else Obj.magic (Obj.field v (Obj.size v - 1)) in
    let get_field lident =
      if is_static then
        match lident with
        | Longident.Lident n -> SMap.find n fds
        | _ -> let (_, id, _) = env_get_field env lident in id
      else
        find_field fnames 0 (lident_name lident)
    in
    List.fold_left (fun env ({ txt = lident }, p) -> pattern_bind env p (Obj.field v (get_field lident))) env rp
  | Ppat_array _ -> assert false
  | Ppat_or (p1, p2) ->
    (try pattern_bind env p1 v with Match_fail -> pattern_bind env p2 v)
  | Ppat_constraint (p, _) -> pattern_bind env p v
  | Ppat_type _ -> assert false
  | Ppat_lazy _ -> assert false
  | Ppat_unpack { txt = name } ->
    assert (Obj.tag v = tag_ModVal); env_set_module name (Obj.magic (Obj.field v 0)) env
  | Ppat_exception _ -> raise Match_fail
  | Ppat_extension _ -> assert false
  | Ppat_open _ -> assert false

(*
   Returns the environment obtained by matching the pattern list [l] with the fields of the object [v],
   starting from field [i], in environment [env].

   In case of matching failure, raises [Match_fail]
*)
and pattern_bind_list env l v i =
  match l with
  | [] -> env
  | p :: l -> pattern_bind_list (pattern_bind env p (Obj.field v i)) l v (i + 1)

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
  | [] -> (match arg with Ok _ -> raise Match_fail | Error v -> raise (InternalException v))
  | c :: cl ->
    match pattern_bind_checkexn env c.pc_lhs arg with
    | exception Match_fail -> eval_match env cl arg
    | nenv ->
      let guard_ok =
        match c.pc_guard with
        | None -> true
        | Some guard -> is_true (eval_expr nenv guard)
      in
      if guard_ok then
        eval_expr nenv c.pc_rhs
      else
        eval_match env cl arg

and eval_module_expr env me =
  match me.pmod_desc with
  | Pmod_ident { txt = lident } -> env_get_module env lident
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
    let r = eval_expr env e in
    assert (Obj.tag r = tag_ModVal);
    Obj.magic (Obj.field r 0)
  | Pmod_extension _ -> assert false

and eval_structitem init_ignored env it =
  match it.pstr_desc with
  | Pstr_eval (e, _) ->
    let _ = eval_expr env e in
    (* Format.printf "%a@." pp_print_value v; *)
    env
  | Pstr_value (f, vals) ->
    if f = Nonrecursive then
      List.fold_left (bind_value env) env vals
    else
      let er = ref env in
      let nenv = List.fold_left (bind_value_rec er) env vals in
      er := nenv; nenv
  | Pstr_primitive { pval_name = { txt = name } ; pval_prim = l } ->
    let prim_name = List.hd l in
    let prim =
      try SMap.find prim_name prims with
        Not_found ->
        if debug then Format.eprintf "Unknown primitive: %s@." prim_name;
        mkprim (fun _ -> failwith ("Unimplemented: " ^ prim_name)) 1
    in
    env_set_value name prim env
  | Pstr_type (_, tl) ->
    List.fold_left (fun env t ->
        match t.ptype_kind with
        | Ptype_variant l ->
          let (_, _, env) = List.fold_left (fun (u, v, env) cd ->
              match cd.pcd_args with
              | Pcstr_tuple [] -> (u + 1, v, env_set_constr cd.pcd_name.txt (u, CTuple 0, false) env)
              | Pcstr_tuple l -> (u, v + 1, env_set_constr cd.pcd_name.txt (v, CTuple (List.length l), false) env)
              | Pcstr_record l ->
                let m = snd (List.fold_left (fun (i, m) field -> (i + 1, SMap.add field.pld_name.txt i m)) (0, SMap.empty) l) in
                (u, v + 1, env_set_constr cd.pcd_name.txt (v, CRecord (List.map (fun f -> f.pld_name.txt) l, m), false) env)
            ) (0, 0, env) l in
          env
        | Ptype_record l ->
          let fnames = List.map (fun f -> f.pld_name.txt) l in
          let is_static = List.mem t.ptype_name.txt static_records in
          let (_, mp) = List.fold_left (fun (i, mp) f -> (i + 1, SMap.add f i mp)) (0, SMap.empty) fnames in
          let (_, env) = List.fold_left (fun (i, env) f ->
              (i + 1, env_set_field f (is_static, i, mp) env)
            ) (0, env) fnames
          in
          env
        | _ -> env
      ) env tl
  | Pstr_typext _ -> env
  | Pstr_exception { pext_name = { txt = name } ; pext_kind = k } ->
    begin
      match k with
      | Pext_decl (typearg, _) ->
        let d = !exn_id in
        incr exn_id;
        begin
          match typearg with
          | Pcstr_tuple l -> env_set_constr name (d, CTuple (List.length l), true) env
          | Pcstr_record l ->
            let m = snd (List.fold_left (fun (i, m) field -> (i + 1, SMap.add field.pld_name.txt i m)) (0, SMap.empty) l) in
            env_set_constr name (d, CRecord (List.map (fun f -> f.pld_name.txt) l, m), true) env
        end
      | Pext_rebind { txt = path } -> env_set_constr name (env_get_constr env path) env
    end
  | Pstr_module { pmb_name = { txt = name } ; pmb_expr = me } ->
    begin
      match init_ignored with
      | None -> env_set_module name (eval_module_expr env me) env
      | Some ign ->
        try env_set_module name (eval_module_expr env me) env
        with Not_found ->
          assert (match me.pmod_desc with Pmod_ident { txt = Longident.Lident s } -> s = name | _ -> false);
          ign := SSet.add name !ign;
          env
    end
  | Pstr_recmodule _ -> assert false
  | Pstr_modtype _ -> env
  | Pstr_open { popen_lid = { txt = lident } } ->
    (match env_get_module env lident with
     | Module (venv, menv, cenv, fenv) -> env_extend false env (venv, menv, cenv, fenv)
     | Functor _ -> assert false)
  | Pstr_class _ -> assert false
  | Pstr_class_type _ -> assert false
  | Pstr_include { pincl_mod = me } ->
    let m = eval_module_expr env me in
    (match m with
     | Module (venv, menv, cenv, fenv) -> env_extend true env (venv, menv, cenv, fenv)
     | Functor _ -> assert false)
  | Pstr_attribute _ -> env
  | Pstr_extension _ -> assert false

and eval_structure_ init_ignored env str =
  match str with
  | [] -> env
  | it :: str -> eval_structure_ init_ignored (eval_structitem init_ignored env it) str

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
let () = eval_expr_fun := eval_expr

let parse filename =
  let inc = open_in filename in
  let lexbuf = Lexing.from_channel inc in
  let parsed = Parse.implementation lexbuf in
  close_in inc;
  parsed

let z x = x

let stdlib_modules = [
  ("Sys", "sys.ml", z);
  ("Seq", "seq.ml", z);
  ("List", "list.ml", z);
  ("Set", "set.ml", z);
  ("Map", "map.ml", z);
  ("Char", "char.ml", z);
  ("Bytes", "bytes.ml", z);
  ("String", "string.ml", z);
  ("Buffer", "buffer.ml", z);
  ("CamlinternalFormatBasics", "camlinternalFormatBasics.ml", z);
  ("CamlinternalFormat", "camlinternalFormat.ml", (fun env ->
       fmt_ebb_of_string_fct := env_get_value env (Longident.Lident "fmt_ebb_of_string"); env));
  ("Printf", "printf.ml", z);
  ("Format", "format.ml", z);
  ("Obj", "obj.ml", z);
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
  ("Marshal", "marshal.ml", z);
]

let stdlib_path = "/home/nathanael/.opam/4.07.0/lib/ocaml"
let stdlib_modules = List.map (fun (n, p, modifier) -> (n, stdlib_path ^ "/" ^ p, modifier)) stdlib_modules

let load_modules env modules =
  List.fold_left (fun env (modname, modpath, modifier) ->
      if debug then Format.eprintf "Loading %s@." modname;
      let module_contents = modifier (eval_structure None env (parse modpath)) in
      env_set_module modname (make_module module_contents) env
    ) env modules

let init_env =
  let stdlib_main = parse (stdlib_path ^ "/stdlib.ml") in
  let ign = ref SSet.empty in
  let env = eval_structure (Some ign) !initial_env stdlib_main in
  let env = load_modules env stdlib_modules in
  env_set_module "Stdlib" (make_module env) env

let compiler_modules = [
  (* Utils *)
  ("Config", "utils/config.ml", z);
  ("Misc", "utils/misc.ml", z);
  ("Identifiable", "utils/identifiable.ml", z);
  ("Numbers", "utils/numbers.ml", z);
  ("Arg_helper", "utils/arg_helper.ml", z);
  ("Clflags", "utils/clflags.ml", z);
  ("Tbl", "utils/tbl.ml", z);
  ("Profile", "utils/profile.ml.noprof", z);
  ("Terminfo", "utils/terminfo.ml", z);
  ("Ccomp", "utils/ccomp.ml", z);
  ("Warnings", "utils/warnings.ml", z);
  ("Consistbl", "utils/consistbl.ml", z);
  ("Strongly_connected_components", "utils/strongly_connected_components.ml", z);
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
  ("Main", "driver/main.ml", z);
]

let compiler_path = (*"/home/nathanael/.opam/4.07.0/lib/ocaml/compiler-libs"*) "/home/nathanael/Projects/ocaml"
let compiler_modules = List.map (fun (n, p, modifier) -> (n, compiler_path ^ "/" ^ p, modifier)) compiler_modules

(* let _ = eval_structure None init_env parsed *)
let () =
  try ignore (load_modules init_env compiler_modules)
  with InternalException e -> Format.eprintf "Code raised exception.@." (* pp_print_value e*)
