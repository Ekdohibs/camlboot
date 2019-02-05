open Asttypes
open Parsetree

let trace = false
let tracearg_from = 742740000
let tracecur = ref 0
let debug = false

module SMap = struct
  (* Unbalanced maps of strings *)
  type t = Empty | Node of 'a t * string * 'a * 'a t
  let is_empty m = (m = Empty)
  let empty = Empty
  let rec add key v m = match m with
    | Empty -> Node (Empty, key, v, Empty)
    | Node (l, key1, v1, r) ->
      if key1 < key then
        Node (l, key1, v1, add key v r)
      else if key1 = key then
        Node (l, key, v, r)
      else
        Node (add key v l, key1, v1, r)
  let rec find key m = match m with
    | Empty -> raise Not_found
    | Node (l, key1, v1, r) ->
      if key1 < key then
        find key r
      else if key1 = key then
        v1
      else
        find key l
  let rec mem key m = match m with
    | Empty -> false
    | Node (l, key1, _, r) ->
      if key1 < key then
        mem key r
      else if key1 = key then
        true
      else
        mem key l
  let rec pop_min_binding r = match r with
    | Empty -> assert false
    | Node (l, key, v, r) ->
      if l = Empty then (key, v, r)
      else
        let (key1, v1, l1) = pop_min_binding l in
        (key1, v1, Node (l1, key, v, r))
  let merge l r = match r with
    | Empty -> l
    | Node _ -> let key, v, r = pop_min_binding r in
      Node (l, key, v, r)
  let rec remove key m = match m with
    | Empty -> Empty
    | Node (l, key1, v1, r) ->
      if key1 < key then
        Node (l, key1, v1, remove key r)
      else if key1 = key then
        merge l r
      else
        Node (remove key l, key1, v1, r)
  let rec map f m = match m with
    | Empty -> Empty
    | Node (l, key, v, r) -> Node (map f l, key, f v, map f r)
  let rec filter f m = match m with
    | Empty -> Empty
    | Node (l, key, v, r) ->
      if f key v then
        Node (filter f l, key, v, filter f r)
      else
        merge (filter f l) (filter f r)
  let rec fold f m x =
    match m with
    | Empty -> x
    | Node (l, key, v, r) ->
      fold f r (f key v (fold f l x))
  let rec fold1 f a m x =
    match m with
    | Empty -> x
    | Node (l, key, v, r) ->
      fold1 f a r (f a key v (fold1 f a l x))
  (* TODO *)
end

let tag_Fun = 230
let tag_Function = 231
let tag_ModVal = 232
let tag_SeqOr = 233
let tag_SeqAnd = 234
let tag_Lz = 235
let tag_Lz_computed = 236
let tag_Fun_with_extra_args = 237
let tag_Prim = 238

type value = Obj.t

(* Everything in the environment has a boolean saying whether it should be exposed
   in the module that is produced.
   Thus, the difference between [open] and [include] is that [open] adds the bindings
   of the module being opened with a [false] boolean (not exposed outside of the current unit),
   while [include] will add the bindings with a [true] boolean, indicating that they should
   be exposed when we pack the unit into a module.
 *)
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

(* The name of the records that are defined as static and must have the same layout as in standard OCaml. *)
let static_records = [
  "parse_tables"; "parser_env"; (* for the parse_engine primitive *)
  "lex_tables"; "lexbuf"; "position"; (* for the lexing primitives *)
  "ref"; (* for %makemutable *)
  "compilation_unit"; "library"; (* for cmo, cma format *)
]

exception InternalException of value

let rec read_caml_int_loop s base i c =
  if i = String.length s then c
  else
    let x = s.[i] in
    let c =
      if '0' <= x && x <= '9' then
        caml_int64_add (caml_int64_mul base c) (caml_int64_of_int (int_of_char x - int_of_char '0'))
      else if 'a' <= x && x <= 'f' then
        caml_int64_add (caml_int64_mul base c) (caml_int64_of_int (int_of_char x - int_of_char 'a' + 10))
      else if 'A' <= x && x <= 'F' then
        caml_int64_add (caml_int64_mul base c) (caml_int64_of_int (int_of_char x - int_of_char 'A' + 10))
      else if x = '_' then c
      else ((* Format.eprintf "FIXME literal: %s@." s; *) assert false)
    in
    read_caml_int_loop s base (i + 1) c

let read_caml_int s =
  let c = ref (caml_int64_of_int 0) in
  let sign, init = if String.length s > 0 && s.[0] = '-' then (caml_int64_of_int (-1), 1) else (caml_int64_of_int 1, 0) in
  let base, init =
    if String.length s >= init + 2 && s.[init] = '0' then
      let c = s.[init + 1] in
      let b = if c = 'x' || c = 'X' then 16 else if c = 'b' || c = 'B' then 2 else if c = 'o' || c = 'O' then 8 else assert false in
      (caml_int64_of_int b, init + 2)
    else
      (caml_int64_of_int 10, init)
  in
  caml_int64_mul sign (read_caml_int_loop s base init (caml_int64_of_int 0))

let value_of_constant cst = match cst with
  | Pconst_integer (s, c) ->
    (match c with
     | None -> Obj.repr (caml_int64_to_int (read_caml_int s))
     | Some c ->
       if c = 'l' then Obj.repr (caml_int64_to_int32 (read_caml_int s))
       else if c = 'L' then Obj.repr (read_caml_int s)
       else if c = 'n' then Obj.repr (caml_int64_to_nativeint (read_caml_int s))
       else assert false)
  | Pconst_char c -> Obj.repr (int_of_char c)
  | Pconst_float (f, _) -> Obj.repr (caml_float_of_string f)
  | Pconst_string (s, _) -> Obj.repr (Bytes.of_string s)

let value_equal (v1 : value) (v2 : value) = v1 = v2
let value_compare (v1 : value) (v2 : value) = compare v1 v2

let value_lt v1 v2 = value_compare v1 v2 < 0
let value_le v1 v2 = value_compare v1 v2 <= 0
let value_gt v1 v2 = value_compare v1 v2 > 0
let value_ge v1 v2 = value_compare v1 v2 >= 0

exception Match_fail

let is_true (v : value) = Obj.magic v

let rec lident_name li = match li with
  | Longident.Lident s -> s
  | Longident.Ldot (_, s) -> s
  | Longident.Lapply (l1, l2) -> lident_name l2

let unit = Obj.repr ()

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
  | Pexp_constraint (e, _) -> eval_fun_or_function envref e
  | Pexp_coerce (e, _, _) -> eval_fun_or_function envref e
  | Pexp_newtype (_, e) -> eval_fun_or_function envref e
  | _ -> failwith "unsupported rhs of rec"

let rec env_get_module env lident =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str env.env_modules)
     with Not_found ->
       (* if debug then Format.eprintf "Module not found in env: %s@." str ; *) raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env ld in
    (match md with
     | Functor _ -> failwith "Ldot tried to access functor"
     | Module (_, md, _, _) ->
       try SMap.find str md
       with Not_found -> (* if debug then Format.eprintf "Module not found in submodule: %s@." (String.concat "." (Longident.flatten lident)) ; *) raise Not_found)
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_value env lident =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str env.env_vars)
     with Not_found ->
       (* if debug then Format.eprintf "Variable not found in env: %s@." str; *) raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env ld in
    (match md with
     | Functor _ -> failwith "Ldot tried to access functor"
     | Module (md, _, _, _) ->
       try SMap.find str md
       with Not_found -> (* if debug then Format.eprintf "Value not found in submodule: %s@." (String.concat "." (Longident.flatten lident)); *) raise Not_found)
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_constr env lident =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str env.env_constructors)
     with Not_found ->
       (* if debug then Format.eprintf "Constructor not found in env: %s@." str; *) raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env ld in
    (match md with
     | Functor _ -> failwith "Ldot tried to access functor"
     | Module (_, _, md, _) ->
       try SMap.find str md
       with Not_found -> (* if debug then Format.eprintf "Constructor not found in submodule: %s@." (String.concat "." (Longident.flatten lident)); *) raise Not_found)
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_field env lident =
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str env.env_fields)
     with Not_found ->
       (* This field might be specified by type disambiguation: say it is not static *)
       (false, 0, SMap.empty)
    )
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env ld in
    (match md with
     | Functor _ -> failwith "Ldot tried to access functor"
     | Module (_, _, _, md) ->
       try SMap.find str md
       with Not_found -> (* if debug then Format.eprintf "Field not found in submodule: %s@." (String.concat "." (Longident.flatten lident)); *) raise Not_found)
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_set_value key v env =
  { env with env_vars = SMap.add key (true, v) env.env_vars }

let env_set_module key m env =
  { env with env_modules = SMap.add key (true, m) env.env_modules }

let env_set_constr key c env =
  { env with env_constructors = SMap.add key (true, c) env.env_constructors }

let env_set_field key f env =
  { env with env_fields = SMap.add key (true, f) env.env_fields }

let env_extend exported env ev1 =
  let (ve1, me1, ce1, fe1) = ev1 in
  {
    env_vars = SMap.fold1 (fun exported key v ve -> SMap.add key (exported, v) ve) exported ve1 env.env_vars ;
    env_modules = SMap.fold1 (fun exported key m me -> SMap.add key (exported, m) me) exported me1 env.env_modules ;
    env_constructors = SMap.fold1 (fun exported key c ce -> SMap.add key (exported, c) ce) exported ce1 env.env_constructors ;
    env_fields = SMap.fold1 (fun exported key f fe -> SMap.add key (exported, f) fe) exported fe1 env.env_fields ;
  }

let make_module env =
  let ve = SMap.map snd (SMap.filter (fun _ bb -> fst bb) env.env_vars) in
  let me = SMap.map snd (SMap.filter (fun _ bb -> fst bb) env.env_modules) in
  let ce = SMap.map snd (SMap.filter (fun _ bb -> fst bb) env.env_constructors) in
  let fe = SMap.map snd (SMap.filter (fun _ bb -> fst bb) env.env_fields) in
  Module (ve, me, ce, fe)

let prevent_export env =
  {
    env_vars = SMap.map (fun xx -> (false, snd xx)) env.env_vars ;
    env_modules = SMap.map (fun xx -> (false, snd xx)) env.env_modules ;
    env_constructors = SMap.map (fun xx -> (false, snd xx)) env.env_constructors ;
    env_fields = SMap.map (fun xx -> (false, snd xx)) env.env_fields ;
  }

let empty_env = {
  env_vars = SMap.empty ;
  env_modules = SMap.empty ;
  env_constructors = SMap.empty ;
  env_fields = SMap.empty ;
}

let apply_ref = ref (fun x y -> assert false)
let eval_expr_ref = ref (fun x y -> assert false)

let mkprim f (arity : int) =
  let r = Obj.new_block tag_Prim 2 in
  Obj.set_field r 0 (Obj.repr f);
  Obj.set_field r 1 (Obj.repr arity);
  r

external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash"
external open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"
external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"
external close_desc : int -> unit = "caml_sys_close"
external set_out_channel_name: out_channel -> string -> unit = "caml_ml_set_channel_name"
external out_channels_list : unit -> out_channel list = "caml_ml_out_channels_list"
external unsafe_output : out_channel -> bytes -> int -> int -> unit = "caml_ml_output_bytes"
external unsafe_output_string : out_channel -> string -> int -> int -> unit = "caml_ml_output"
(* external set_in_channel_name: in_channel -> string -> unit = "caml_ml_set_channel_name"
external unsafe_input : in_channel -> bytes -> int -> int -> int = "caml_ml_input"
external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"
external random_seed : unit -> int array = "caml_sys_random_seed"
external digest_unsafe_string : string -> int -> int -> string = "caml_md5_string"
external marshal_to_channel : out_channel -> 'a -> unit list -> unit = "caml_output_value"
external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external input_scan_line : in_channel -> int = "caml_ml_input_scan_line"
external caml_register_named_value : string -> Obj.t -> unit = "caml_register_named_value"
external caml_ml_set_channel_name : Obj.t -> string -> unit = "caml_ml_set_channel_name"
external caml_ml_close_channel : Obj.t -> unit = "caml_ml_close_channel" *)
external lex_engine : Lexing.lex_tables -> int -> Lexing.lexbuf -> int = "caml_lex_engine"
external new_lex_engine : Lexing.lex_tables -> int -> Lexing.lexbuf -> int = "caml_new_lex_engine"

external parse_engine : Parsing.parse_tables -> Obj.t -> Obj.t -> Obj.t -> Obj.t = "caml_parse_engine"
let last_parse_tables = ref (Obj.repr 0)
let last_parse_tables_converted = ref (Obj.repr 0)
let parse_engine_wrapper tables env input token =
(*  let parse_tables_converted =
    if tables == !last_parse_tables then
      Obj.magic !last_parse_tables_converted
    else begin
      last_parse_tables := tables;
      let tables : Parsing.parse_tables = Obj.magic tables in
      let cvrt = {
        tables with
        Parsing.actions = Array.map (fun f -> fun pe -> !apply_ref f [(Nolabel, Obj.repr pe)]) (Obj.magic tables.Parsing.actions)
      } in
      last_parse_tables_converted := Obj.repr cvrt;
      cvrt
    end
  in
    parse_engine parse_tables_converted env input token *) parse_engine tables env input token

let id x = x

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

let prims = [
  ("%apply", mkprim (fun vf v -> let ar = !apply_ref in ar vf [(Nolabel, v)]) 2);
  ("%revapply", mkprim (fun v vf -> let ar = !apply_ref in ar vf [(Nolabel, v)]) 2);
  ("%raise", mkprim (fun v -> raise (InternalException v)) 1);
  ("%reraise", mkprim (fun v -> raise (InternalException v)) 1);
  ("%raise_notrace", mkprim (fun v -> raise (InternalException v)) 1);
  ("%sequand", (let r = Obj.new_block tag_SeqAnd 1 in Obj.set_field r 0 (Obj.repr 0); r));
  ("%sequor", (let r = Obj.new_block tag_SeqOr 1 in Obj.set_field r 0 (Obj.repr 0); r));
  ("%boolnot", mkprim not 1);
  ("%negint", mkprim uminus 1);
  ("%succint", mkprim succ 1);
  ("%predint", mkprim pred 1);
  ("%addint", mkprim plus 2);
  ("%subint", mkprim minus 2);
  ("%mulint", mkprim times 2);
  ("%divint", mkprim div_ 2);
  ("%modint", mkprim mod_ 2);
  ("%andint", mkprim land_ 2);
  ("%orint", mkprim lor_ 2);
  ("%xorint", mkprim lxor_ 2);
  ("%lslint", mkprim lsl_ 2);
  ("%lsrint", mkprim lsr_ 2);
  ("%asrint", mkprim asr_ 2);
  ("%addfloat", mkprim caml_add_float 2);
  ("%subfloat", mkprim caml_sub_float 2);
  ("%mulfloat", mkprim caml_mul_float 2);
  ("%divfloat", mkprim caml_div_float 2);
  ("%floatofint", mkprim caml_float_of_int 1);
  ("%intoffloat", mkprim caml_int_of_float 1);
  ("caml_float_of_string", mkprim caml_float_of_string 1);
  ("%lessthan", mkprim value_lt 2);
  ("%lessequal", mkprim value_le 2);
  ("%greaterthan", mkprim value_gt 2);
  ("%greaterequal", mkprim value_ge 2);
  ("%compare", mkprim value_compare 2);
  ("%equal", mkprim value_equal 2);
  ("%notequal", mkprim (fun x y -> not (value_equal x y)) 2);
  ("%eq", mkprim caml_eq 2);
  ("%noteq", mkprim caml_noteq 2);
  ("%identity", mkprim (fun x -> x) 1);
  ("caml_register_named_value", mkprim caml_register_named_value 2);
  ("caml_int64_float_of_bits", mkprim caml_int64_float_of_bits 1);
  ("caml_ml_open_descriptor_out", mkprim caml_ml_open_descriptor_out 1);
  ("caml_ml_open_descriptor_in", mkprim caml_ml_open_descriptor_in 1);
  ("caml_sys_open", mkprim caml_sys_open 3);
  ("caml_sys_close", mkprim caml_sys_close 1);
  ("caml_ml_set_channel_name", mkprim caml_ml_set_channel_name 2);
  ("caml_ml_close_channel", mkprim caml_ml_close_channel 1);
  ("caml_ml_out_channels_list", mkprim caml_ml_out_channels_list 1);
  ("caml_ml_output_bytes", mkprim caml_ml_output_bytes 4);
  ("caml_ml_output", mkprim caml_ml_output 4);
  ("caml_ml_output_int", mkprim caml_ml_output_int 2);
  ("caml_ml_output_char", mkprim caml_ml_output_char 2);
  ("caml_ml_flush", mkprim caml_ml_flush 1);
  ("caml_ml_input_char", mkprim caml_ml_input_char 1);
  ("caml_ml_input_int", mkprim caml_ml_input_int 1);
  ("caml_ml_input_scan_line", mkprim caml_ml_input_scan_line 1);
  ("caml_ml_input", mkprim caml_ml_input 4);
  ("caml_ml_seek_in", mkprim caml_ml_seek_in 2);
  ("caml_ml_pos_out", mkprim caml_ml_pos_out 1);
  ("caml_ml_pos_in", mkprim caml_ml_pos_in 1);
  ("caml_ml_seek_out", mkprim caml_ml_seek_out 2);
  ("%makemutable", mkprim ref 1);
  ("%field0", mkprim fst 1);
  ("%field1", mkprim snd 1);
  ("%setfield0", mkprim ref_set 2);
  ("%incr", mkprim incr 1);
  ("%decr", mkprim decr 1);
  ("%ignore", mkprim (fun _ -> ()) 1);
  ("caml_format_int", mkprim caml_format_int 2);
  ("caml_format_float", mkprim caml_format_float 2);
  ("caml_int_of_string", mkprim caml_int_of_string 1);
  ("caml_output_value", mkprim caml_output_value 3);
  ("caml_output_value_to_buffer", mkprim caml_output_value_to_buffer 5);
  ("caml_input_value", mkprim caml_input_value 1);
  ("caml_sys_exit", mkprim caml_sys_exit 1);
  ("caml_parse_engine", mkprim parse_engine_wrapper 4);
  ("caml_lex_engine", mkprim lex_engine 3);
  ("caml_new_lex_engine", mkprim new_lex_engine 3);

  (* Sys *)
  ("caml_sys_get_argv", mkprim caml_sys_get_argv 1);
  ("caml_sys_get_config", mkprim caml_sys_get_config 1);
  ("%big_endian", mkprim caml_sys_const_big_endian 1);
  ("%word_size", mkprim (fun _ -> 64) 1);
  ("%int_size", mkprim (fun _ -> 64) 1);
  ("%max_wosize", mkprim (fun _ -> 1000000) 1);
  ("%ostype_unix", mkprim (fun _ -> false) 1);
  ("%ostype_win32", mkprim (fun _ -> false) 1);
  ("%ostype_cygwin", mkprim (fun _ -> false) 1);
  ("%backend_type", mkprim (fun _ -> Sys.Other "Interpreter") 1);
  ("caml_sys_getenv", mkprim (fun _ -> raise (InternalException not_found_exn)) 1);
  ("caml_sys_file_exists", mkprim caml_sys_file_exists 1);
  ("caml_sys_getcwd", mkprim caml_sys_getcwd 1);
  ("caml_sys_rename", mkprim caml_sys_rename 2);
  ("caml_sys_remove", mkprim caml_sys_remove 1);
  ("caml_sys_system_command", mkprim (fun x -> assert false) 1);

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
         let ev = !eval_expr_ref in
         let r = ev (Obj.magic (Obj.field v 0)) (Obj.magic (Obj.field v 1)) in
         Obj.set_tag v tag_Lz_computed;
         Obj.set_field v 0 r;
         r
       end
     ) 1);

  (* Int64 *)
  ("%int64_neg", mkprim caml_int64_neg 1);
  ("%int64_add", mkprim caml_int64_add 2);
  ("%int64_sub", mkprim caml_int64_sub 2);
  ("%int64_mul", mkprim caml_int64_mul 2);
  ("%int64_div", mkprim caml_int64_div 2);
  ("%int64_mod", mkprim caml_int64_mod 2);
  ("%int64_and", mkprim caml_int64_and 2);
  ("%int64_or", mkprim caml_int64_or 2);
  ("%int64_xor", mkprim caml_int64_xor 2);
  ("%int64_lsl", mkprim caml_int64_shift_left 2);
  ("%int64_lsr", mkprim caml_int64_shift_right_unsigned 2);
  ("%int64_asr", mkprim caml_int64_shift_right 2);
  ("%int64_of_int", mkprim caml_int64_of_int 1);
  ("%int64_to_int", mkprim caml_int64_to_int 1);
  ("caml_int64_of_string", mkprim caml_int64_of_string 1);

  (* Int32 *)
  ("caml_int32_of_string", mkprim caml_int32_of_string 1);
  ("%int32_neg", mkprim caml_int32_neg 1);

  (* Nativeint *)
  ("%nativeint_neg", mkprim caml_nativeint_neg 1);
  ("%nativeint_add", mkprim caml_nativeint_add 2);
  ("%nativeint_sub", mkprim caml_nativeint_sub 2);
  ("%nativeint_mul", mkprim caml_nativeint_mul 2);
  ("%nativeint_div", mkprim caml_nativeint_div 2);
  ("%nativeint_mod", mkprim caml_nativeint_mod 2);
  ("%nativeint_and", mkprim caml_nativeint_and 2);
  ("%nativeint_or", mkprim caml_nativeint_or 2);
  ("%nativeint_xor", mkprim caml_nativeint_xor 2);
  ("%nativeint_lsl", mkprim caml_nativeint_shift_left 2);
  ("%nativeint_lsr", mkprim caml_nativeint_shift_right_unsigned 2);
  ("%nativeint_asr", mkprim caml_nativeint_shift_right 2);
  ("%nativeint_of_int", mkprim caml_nativeint_of_int 1);
  ("%nativeint_to_int", mkprim caml_nativeint_to_int 1);
  ("caml_nativeint_of_string", mkprim caml_nativeint_of_string 1);

  (* Array *)
  ("caml_make_vect", mkprim caml_make_vect 2);
  ("%array_length", mkprim Array.length 1);
  ("caml_array_sub", mkprim caml_array_sub 3);
  ("%array_safe_get", mkprim caml_array_get 2);
  ("%array_unsafe_get", mkprim caml_array_unsafe_get 2);
  ("%array_safe_set", mkprim caml_array_set 3);
  ("%array_unsafe_set", mkprim caml_array_unsafe_set 3);
  ("caml_array_blit", mkprim caml_array_blit 5);
  ("caml_array_append", mkprim caml_array_append 2);

  (* Hashtbl *)
  ("caml_hash", mkprim caml_hash 4);

  (* Weak *)
  ("caml_weak_create", mkprim caml_weak_create 1);
  ("caml_weak_get", mkprim caml_weak_get 2);
  ("caml_weak_get_copy", mkprim caml_weak_get_copy 2);
  ("caml_weak_set", mkprim caml_weak_set 3);
  ("caml_weak_check", mkprim caml_weak_check 2);
  ("caml_weak_blit", mkprim caml_weak_blit 5);

  (* Random *)
  ("caml_sys_random_seed", mkprim caml_sys_random_seed 1);

  (* Digest *)
  ("caml_md5_string", mkprim caml_md5_string 3);
  ("caml_md5_chan", mkprim caml_md5_chan 2);

  (* Ugly *)
  ("%obj_size", mkprim Obj.size 1);
  ("caml_obj_block", mkprim caml_obj_block 2);
  ("caml_obj_tag", mkprim caml_obj_tag 1);
  ("%obj_is_int", mkprim Obj.is_int 1);
  ("%obj_field", mkprim Obj.field 2);
  ("%obj_set_field", mkprim Obj.set_field 3);
]

let prims = List.fold_left (fun env nv -> let name, v = nv in SMap.add name v env) SMap.empty prims
let hash_variant_name (name : string) = land_ (Hashtbl.hash name) (lsl_ 1 30 - 1)

let fmt_ebb_of_string_fct = ref (Obj.repr 0)

let mkblock tag l =
  let r = Obj.new_block tag (List.length l) in
  List.iteri1 (fun r i x -> Obj.set_field r i x) r l;
  r

let rec obj_copy obj1 obj2 i j =
  if i = j then ()
  else (Obj.set_field obj2 i (Obj.field obj1 i); obj_copy obj1 obj2 (i + 1) j)

let rec find_field fnames idx name =
  match fnames with
  | [] -> assert false
  | fname :: fnames -> if fname = name then idx else find_field fnames (idx + 1) name

(*
let rec apply_loop1 f clos i j =
  if i = j then f
  else let w = Obj.magic f in apply_loop1 (w (Obj.field clos i)) clos (i + 1) j

let prim_complete_apply vf arg =
  let w = Obj.magic (apply_loop1 (Obj.field vf 0) vf 2 (Obj.size vf)) in
  w arg
*)

let prim_complete_apply vf arg =
  let f = Obj.magic (Obj.field vf 0) in
  let arity = Obj.magic (Obj.field vf 1) in
  match arity with
  | 1 -> f arg
  | 2 -> f (Obj.field vf 2) arg
  | 3 -> f (Obj.field vf 2) (Obj.field vf 3) arg
  | 4 -> f (Obj.field vf 2) (Obj.field vf 3) (Obj.field vf 4) arg
  | 5 -> f (Obj.field vf 2) (Obj.field vf 3) (Obj.field vf 4) (Obj.field vf 5) arg
  | _ -> assert false

let rec apply_labelled vf labarg =
  let lab, arg = labarg in
  assert (Obj.tag vf = tag_Fun);
  let (label : arg_label) = Obj.magic (Obj.field vf 0) in
  let (default : expression option) = Obj.magic (Obj.field vf 1) in
  let (p : pattern) = Obj.magic (Obj.field vf 2) in
  let (e : expression) = Obj.magic (Obj.field vf 3) in
  let (fenv : env ref) = Obj.magic (Obj.field vf 4) in
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

and apply_optional_noarg vf =
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

and apply_one hlwl vf arg =
  let (has_labelled, with_label) = hlwl in
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
        apply_one hlwl (apply_labelled vf v) arg
      end else
        eval_expr (pattern_bind !fenv p arg) e
    | Optional s ->
      if has_labelled && SMap.mem s !with_label then begin
        let v = SMap.find s !with_label in
        with_label := SMap.remove s !with_label;
        apply_one hlwl (apply_labelled vf v) arg
      end else
        apply_one hlwl (apply_optional_noarg vf) arg
  else if tag = tag_Function then
    let cl = Obj.magic (Obj.field vf 0) in
    let fenv = Obj.magic (Obj.field vf 1) in
    eval_match !fenv cl (Ok arg)
  else if tag = tag_Prim then
    let (arity : int) = Obj.magic (Obj.field vf 1) in
    let current_args = Obj.size vf - 2 in
    if current_args + 1 = arity then
      prim_complete_apply vf arg
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

and apply_loop2 with_label vf =
  if SMap.is_empty !with_label then
    vf
  else if Obj.tag vf = tag_Fun && Obj.magic (Obj.field vf 0) <> Nolabel then
    let (lab : arg_label) = Obj.magic (Obj.field vf 0) in
    let s = match lab with Nolabel -> assert false | Labelled s -> s | Optional s -> s in
    if SMap.mem s !with_label then begin
      let v = SMap.find s !with_label in
      with_label := SMap.remove s !with_label;
      apply_loop2 with_label (apply_labelled vf v)
    end else begin
      assert (match lab with Optional _ -> true | _ -> false);
      apply_loop2 with_label (apply_optional_noarg vf)
    end
  else
    let r = Obj.new_block tag_Fun_with_extra_args 3 in
    Obj.set_field r 0 vf;
    Obj.set_field r 1 (Obj.repr []);
    Obj.set_field r 2 (Obj.repr !with_label);
    r

and apply vf args =
  let vf, extral, extram =
    if Obj.tag vf = tag_Fun_with_extra_args then
      (Obj.field vf 0, Obj.magic (Obj.field vf 1), Obj.magic (Obj.field vf 2))
    else
      (vf, [], SMap.empty)
  in
  assert (extral = []);
  let unlabelled = List.map snd (List.filter (fun lb -> fst lb = Nolabel) args) in
  let with_label = ref (List.fold_left (fun wl la -> let (lab, arg) = la in
      match lab with Nolabel -> wl | Optional s -> SMap.add s (lab, arg) wl | Labelled s -> SMap.add s (lab, arg) wl
    ) extram args)
  in
  let has_labelled = not (SMap.is_empty !with_label) in
  if SMap.is_empty !with_label then (* Special case to get tail recursion *)
    List.fold_left1 apply_one (has_labelled, with_label) vf unlabelled
  else
    let vf = List.fold_left1 apply_one (has_labelled, with_label) vf unlabelled in
    apply_loop2 with_label vf

and eval_expr_while env e1 e2 =
  if is_true (eval_expr env e1) then begin
    ignore (eval_expr env e2);
    eval_expr_while env e1 e2
  end else
    unit

and eval_expr_for_up env v1 v2 p e =
  if v1 > v2 then
    unit
  else begin
    ignore (eval_expr (pattern_bind env p (Obj.repr v1)) e);
    eval_expr_for_up env (v1 + 1) v2 p e
  end

and eval_expr_for_down env v1 v2 p e =
  if v1 > v2 then
    unit
  else begin
    ignore (eval_expr (pattern_bind env p (Obj.repr v2)) e);
    eval_expr_for_up env v1 (v2 - 1) p e
  end

and eval_expr_for env flag v1 v2 p e =
  if flag = Upto then
    eval_expr_for_up env v1 v2 p e
  else
    eval_expr_for_down env v1 v2 p e

and eval_expr env expr =
  match expr.pexp_desc with
  | Pexp_ident lident -> env_get_value env lident.txt
  | Pexp_constant c -> value_of_constant c
  | Pexp_let (f, vals, e) ->
    if f = Nonrecursive then
      let nenv = List.fold_left1 bind_value env env vals in
      eval_expr nenv e
    else
      let er = ref env in
      let nenv = List.fold_left1 bind_value_rec er env vals in
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
      let args = List.map1 (fun env le -> let (lab, e) = le in (lab, eval_expr env e)) env l in
      if trace then begin match f.pexp_desc with Pexp_ident lident ->
        (* Format.eprintf "apply %s@." (String.concat "." (Longident.flatten lident.txt)); *)
        print_string (lident_name lident.txt);
        incr tracecur
        (*if !tracecur > tracearg_from then Format.eprintf " %a" (Format.pp_print_list ~pp_sep:(fun ff () -> Format.fprintf ff " ") (fun ff (_, v) -> Format.fprintf ff "%a" pp_print_value v)) args; *)
        | _ -> ()
         end;
      apply fc args
    end
  | Pexp_tuple l ->
    let args = List.map1 eval_expr env l in
    mkblock 0 args
  | Pexp_match (e, cl) -> eval_match env cl (eval_expr_exn env e)
  | Pexp_coerce (e, _, _) -> eval_expr env e
  | Pexp_constraint (e, _) -> eval_expr env e
  | Pexp_sequence (e1, e2) -> let _ = eval_expr env e1 in eval_expr env e2
  | Pexp_while (e1, e2) -> eval_expr_while env e1 e2
  | Pexp_for (p, e1, e2, flag, e3) ->
    let (v1 : int) = Obj.magic (eval_expr env e1) in
    let (v2 : int) = Obj.magic (eval_expr env e2) in
    eval_expr_for env flag v1 v2 p e3
  | Pexp_ifthenelse (e1, e2, e3) ->
    if is_true (eval_expr env e1) then eval_expr env e2 else (match e3 with None -> unit | Some e3 -> eval_expr env e3)
  | Pexp_unreachable -> failwith "reached unreachable"
  | Pexp_try (e, cs) ->
    (try eval_expr env e with
       InternalException v ->
       try eval_match env cs (Ok v) with Match_fail -> raise (InternalException v)
    )
  | Pexp_construct (c, e) ->
    let c = c.txt in
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
             | Pexp_tuple l -> List.map1 eval_expr env l
             | _ -> assert false
           else
             [eval_expr env e]
         | CRecord (fields, _) ->
           match e.pexp_desc with
           | Pexp_record (r, e) ->
             assert (e = None);
             assert (List.length r = List.length fields);
             List.map1 (fun renv x -> let (r, env) = renv in eval_expr env (snd (List.find1 (fun x fe -> lident_name (fst fe).txt = x) x r))) (r, env) fields @ [Obj.repr fields]
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
        let fnames = List.map (fun fe -> lident_name (fst fe).txt) r in
        if not is_static then Obj.set_field r1 (List.length r) (Obj.repr fnames);
        r1, fnames
      | Some e ->
        let r = eval_expr env e in
        let r1 = Obj.new_block 0 (Obj.size r) in
        obj_copy r r1 0 (Obj.size r);
        r1, if is_static then [] else Obj.magic (Obj.field r (Obj.size r - 1))
    in
    List.fold_left1 (fun eiff rc fe ->
        let (env, is_static, fds, fnames) = eiff in
        let lident, ee = fe in
        let lident = lident.txt in
        let field =
          if is_static then
            match lident with
            | Longident.Lident n -> SMap.find n fds
            | _ -> let (_, id, _) = env_get_field env lident in id
          else
            find_field fnames 0 (lident_name lident)
        in
        Obj.set_field rc field (eval_expr env ee); rc
    ) (env, is_static, fds, fnames) base r
  | Pexp_field (e, lident) ->
    let lident = lident.txt in
    let is_static, fieldid, _ = env_get_field env lident in
    let r = eval_expr env e in
    let fieldid =
      if is_static then fieldid else
        let fnames = Obj.magic (Obj.field r (Obj.size r - 1)) in
        find_field fnames 0 (lident_name lident)
    in
    Obj.field r fieldid
  | Pexp_setfield (e1, lident, e2) ->
    let lident = lident.txt in
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
  | Pexp_array l -> Obj.repr (Array.of_list (List.map1 eval_expr env l))
  | Pexp_send _ -> assert false
  | Pexp_new _ -> assert false
  | Pexp_setinstvar _ -> assert false
  | Pexp_override _ -> assert false
  | Pexp_letexception (pext, e) ->
    let k = pext.pext_kind in
    let name = pext.pext_name.txt in
    let nenv =
      match k with
      | Pext_decl (_, typearg) ->
        let arity = match typearg with None -> 0 | Some _ -> 1 in
        let d = !exn_id in incr exn_id; env_set_constr name (d, CTuple arity, true) env
      | Pext_rebind path -> env_set_constr name (env_get_constr env path.txt) env
    in
    eval_expr nenv e
  | Pexp_letmodule (name, me, e) ->
    let name = name.txt in
    let m = eval_module_expr env me in
    eval_expr (env_set_module name m env) e
  | Pexp_assert e ->
    if is_true (eval_expr env e) then unit else
      raise (InternalException (mkblock 0 [Obj.repr assert_failure_id; mkblock 0 [Obj.repr ""; Obj.repr 0; Obj.repr 0]]))
  | Pexp_lazy e ->
    let b = Obj.new_block tag_Lz 2 in
    Obj.set_field b 0 (Obj.repr env);
    Obj.set_field b 1 (Obj.repr e);
    b
  | Pexp_poly _ -> assert false
  | Pexp_newtype (_, e) -> eval_expr env e
  | Pexp_open (_, lident, e) ->
    let lident = lident.txt in
    let nenv = try (match env_get_module env lident with
        | Module (venv, menv, cenv, fenv) -> env_extend false env (venv, menv, cenv, fenv)
        | Functor _ -> assert false) with Not_found -> env (* Module might be a .mli only *)
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
  | Ppat_var s -> env_set_value s.txt v env
  | Ppat_alias (p, s) ->
    env_set_value s.txt v (pattern_bind env p v)
  | Ppat_constant c ->
    if value_equal (value_of_constant c) v then env else raise Match_fail
  | Ppat_interval (c1, c2) ->
    if value_le (value_of_constant c1) v && value_le v (value_of_constant c2) then env else raise Match_fail
  | Ppat_tuple l ->
    assert (Obj.size v = List.length l);
    pattern_bind_list env l v 0
  | Ppat_construct (c, p) ->
    let c = c.txt in
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
        | Some p ->
          (* Anonymous records never use static layout *)
          pattern_bind env p v
    end
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
    List.fold_left1 (fun clos env lp ->
        let v, is_static, fds, fnames = clos in
        let lident, p = lp in
        let lident = lident.txt in
        let field =
          if is_static then
            match lident with
            | Longident.Lident n -> SMap.find n fds
            | _ -> let (_, id, _) = env_get_field env lident in id
          else
            find_field fnames 0 (lident_name lident)
        in
        pattern_bind env p (Obj.field v field)) (v, is_static, fds, fnames) env rp
  | Ppat_array _ -> assert false
  | Ppat_or (p1, p2) ->
    (try pattern_bind env p1 v with Match_fail -> pattern_bind env p2 v)
  | Ppat_constraint (p, _) -> pattern_bind env p v
  | Ppat_type _ -> assert false
  | Ppat_lazy _ -> assert false
  | Ppat_unpack name ->
    let name = name.txt in
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
    let z = try Some (pattern_bind_checkexn env c.pc_lhs arg) with Match_fail -> None in
    match z with
    | None -> eval_match env cl arg
    | Some nenv ->
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
  | Pmod_ident lident -> env_get_module env lident.txt
  | Pmod_structure str -> make_module (eval_structure None env str)
  | Pmod_functor (arg_name, _, e) -> Functor (arg_name.txt, e, env)
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
      List.fold_left1 bind_value env env vals
    else
      let er = ref env in
      let nenv = List.fold_left1 bind_value_rec er env vals in
      er := nenv; nenv
  | Pstr_primitive prim ->
    let l = prim.pval_prim in
    let name = prim.pval_name.txt in
    let prim_name = List.hd l in
    let prim =
      try SMap.find prim_name prims with
        Not_found ->
        (* if debug then Format.eprintf "Unknown primitive: %s@." prim_name; *)
        (* mkprim (fun _ -> failwith ("Unimplemented: " ^ prim_name)) 1 *)
        mkprim (fun _ -> failwith "Unimplemented") 1
    in
    env_set_value name prim env
  | Pstr_type (_, tl) ->
    List.fold_left (fun env t ->
        match t.ptype_kind with
        | Ptype_variant l ->
          let (_, _, env) = List.fold_left (fun uvenv cd -> let (u, v, env) = uvenv in
              match cd.pcd_args with
              | Pcstr_tuple l ->
                if l = [] then
                  (u + 1, v, env_set_constr cd.pcd_name.txt (u, CTuple 0, false) env)
                else
                  (u, v + 1, env_set_constr cd.pcd_name.txt (v, CTuple (List.length l), false) env)
              | Pcstr_record l ->
                let m = snd (List.fold_left (fun im field -> let (i, m) = im in (i + 1, SMap.add field.pld_name.txt i m)) (0, SMap.empty) l) in
                (u, v + 1, env_set_constr cd.pcd_name.txt (v, CRecord (List.map (fun f -> f.pld_name.txt) l, m), false) env)
            ) (0, 0, env) l in
          env
        | Ptype_record l ->
          let fnames = List.map (fun f -> f.pld_name.txt) l in
          let is_static = List.mem t.ptype_name.txt static_records in
          let (_, mp) = List.fold_left (fun imp f -> let (i, mp) = imp in (i + 1, SMap.add f i mp)) (0, SMap.empty) fnames in
          let (_, env) = List.fold_left1 (fun mps ienv f -> let (mp, is_static) = mps in let (i, env) = ienv in
              (i + 1, env_set_field f (is_static, i, mp) env)
            ) (mp, is_static) (0, env) fnames
          in
          env
        | _ -> env
      ) env tl
  | Pstr_typext _ -> env
  | Pstr_exception pext ->
    let k = pext.pext_kind in
    let name = pext.pext_name.txt in
    begin
      match k with
      | Pext_decl (typearg, _) ->
        let d = !exn_id in
        incr exn_id;
        begin
          match typearg with
          | Pcstr_tuple l -> env_set_constr name (d, CTuple (List.length l), true) env
          | Pcstr_record l ->
            let m = snd (List.fold_left (fun im field -> let (i, m) = im in (i + 1, SMap.add field.pld_name.txt i m)) (0, SMap.empty) l) in
            env_set_constr name (d, CRecord (List.map (fun f -> f.pld_name.txt) l, m), true) env
        end
      | Pext_rebind path -> env_set_constr name (env_get_constr env path.txt) env
    end
  | Pstr_module pmb ->
    let me = pmb.pmb_expr in
    let name = pmb.pmb_name.txt in
    begin
      match init_ignored with
      | None -> env_set_module name (eval_module_expr env me) env
      | Some ign ->
        try env_set_module name (eval_module_expr env me) env
        with Not_found ->
          assert (match me.pmod_desc with Pmod_ident li -> (match li.txt with Longident.Lident s -> s = name | _ -> false) | _ -> false);
          ign := SMap.add name () !ign;
          env
    end
  | Pstr_recmodule _ -> assert false
  | Pstr_modtype _ -> env
  | Pstr_open popen ->
    let lident = popen.popen_lid.txt in
    (match env_get_module env lident with
     | Module (venv, menv, cenv, fenv) -> env_extend false env (venv, menv, cenv, fenv)
     | Functor _ -> assert false)
  | Pstr_class _ -> assert false
  | Pstr_class_type _ -> assert false
  | Pstr_include pincl ->
    let me = pincl.pincl_mod in
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

let _ = apply_ref := apply
let _ = eval_expr_ref := eval_expr

let parse filename =
  let inc = open_in filename in
  let lexbuf = Lexing.from_channel inc in
  let parsed = Parser.implementation Lexer.real_token lexbuf in
  close_in inc;
  parsed

let stdlib_modules = [
  ("Sys", "sys.ml");
  ("Seq", "seq.ml");
  ("List", "list.ml");
  ("Set", "set.ml");
  ("Map", "map.ml");
  ("Char", "char.ml");
  ("Bytes", "bytes.ml");
  ("String", "string.ml");
  ("Buffer", "buffer.ml");
  ("CamlinternalFormatBasics", "camlinternalFormatBasics.ml");
  ("CamlinternalFormat", "camlinternalFormat.ml");
  ("Printf", "printf.ml");
  ("Format", "format.ml");
  ("Obj", "obj.ml");
  ("CamlinternalLazy", "camlinternalLazy.ml");
  ("Lazy", "lazy.ml");
  ("Array", "array.ml");
  ("Int64", "int64.ml");
  ("Int32", "int32.ml");
  ("Nativeint", "nativeint.ml");
  ("Digest", "digest.ml");
  ("Random", "random.ml");
  ("Hashtbl", "hashtbl.ml");
  ("Lexing", "lexing.ml");
  ("Parsing", "parsing.ml");
  ("Weak", "weak.ml");
  ("Stack", "stack.ml");
  ("Arg", "arg.ml");
  ("Filename", "filename.ml");
  ("CamlinternalOO", "camlinternalOO.ml");
  ("Marshal", "marshal.ml");
]

let stdlib_path = "/home/nathanael/.opam/4.07.0/lib/ocaml"
let stdlib_modules = List.map (fun np -> let (n, p) = np in (n, stdlib_path ^ "/" ^ p)) stdlib_modules

let load_modules env modules =
  List.fold_left (fun env namepath ->
      let modname, modpath = namepath in
      (* if debug then Format.eprintf "Loading %s@." modname; *)
      let module_contents = eval_structure None env (parse modpath) in
      if modname = "CamlinternalFormat" then
        fmt_ebb_of_string_fct := env_get_value module_contents (Longident.Lident "fmt_ebb_of_string");
      env_set_module modname (make_module module_contents) env
    ) env modules

let init_env =
  let stdlib_main = parse (stdlib_path ^ "/stdlib.ml") in
  let ign = ref SMap.empty in
  let env = eval_structure (Some ign) !initial_env stdlib_main in
  let env = load_modules env stdlib_modules in
  env_set_module "Stdlib" (make_module env) env

let compiler_modules = [
  (* Utils *)
  ("Config", "utils/config.ml");
  ("Misc", "utils/misc.ml");
  ("Identifiable", "utils/identifiable.ml");
  ("Numbers", "utils/numbers.ml");
  ("Arg_helper", "utils/arg_helper.ml");
  ("Clflags", "utils/clflags.ml");
  ("Tbl", "utils/tbl.ml");
  ("Profile", "utils/profile.ml.noprof");
  ("Terminfo", "utils/terminfo.ml");
  ("Ccomp", "utils/ccomp.ml");
  ("Warnings", "utils/warnings.ml");
  ("Consistbl", "utils/consistbl.ml");
  ("Strongly_connected_components", "utils/strongly_connected_components.ml");
  ("Build_path_prefix_map", "utils/build_path_prefix_map.ml");
  ("Targetint", "utils/targetint.ml");

  (* Parsing *)
  ("Asttypes", "parsing/asttypes.mli");
  ("Location", "parsing/location.ml");
  ("Longident", "parsing/longident.ml");
  ("Parsetree", "parsing/parsetree.mli");
  ("Docstrings", "parsing/docstrings.ml");
  ("Syntaxerr", "parsing/syntaxerr.ml");
  ("Ast_helper", "parsing/ast_helper.ml");
  ("Parser", "parsing/parser.ml");
  ("Lexer", "parsing/lexer.ml");
  ("Parse", "parsing/parse.ml");
  ("Printast", "parsing/printast.ml");
  ("Pprintast", "parsing/pprintast.ml");
  ("Ast_mapper", "parsing/ast_mapper.ml");
  ("Ast_iterator", "parsing/ast_iterator.ml");
  ("Attr_helper", "parsing/attr_helper.ml");
  ("Builtin_attributes", "parsing/builtin_attributes.ml");
  ("Ast_invariants", "parsing/ast_invariants.ml");
  ("Depend", "parsing/depend.ml");

  (* Typing *)
  ("Ident", "typing/ident.ml");
  ("Outcometree", "typing/outcometree.mli");
  ("Annot", "typing/annot.mli");
  ("Path", "typing/path.ml");
  ("Primitive", "typing/primitive.ml");
  ("Types", "typing/types.ml");
  ("Btype", "typing/btype.ml");
  ("Oprint", "typing/oprint.ml");
  ("Subst", "typing/subst.ml");
  ("Predef", "typing/predef.ml");
  ("Datarepr", "typing/datarepr.ml");
  ("Cmi_format", "typing/cmi_format.ml");
  ("Env", "typing/env.ml");
  ("Typedtree", "typing/typedtree.ml");
  ("Printtyped", "typing/printtyped.ml");
  ("Ctype", "typing/ctype.ml");
  ("Printtyp", "typing/printtyp.ml");
  ("Includeclass", "typing/includeclass.ml");
  ("Mtype", "typing/mtype.ml");
  ("Envaux", "typing/envaux.ml");
  ("Includecore", "typing/includecore.ml");
  ("TypedtreeIter", "typing/typedtreeIter.ml");
  ("TypedtreeMap", "typing/typedtreeMap.ml");
  ("Tast_mapper", "typing/tast_mapper.ml");
  ("Cmt_format", "typing/cmt_format.ml");
  ("Untypeast", "typing/untypeast.ml");
  ("Includemod", "typing/includemod.ml");
  ("Typetexp", "typing/typetexp.ml");
  ("Printpat", "typing/printpat.ml");
  ("Parmatch", "typing/parmatch.ml");
  ("Stypes", "typing/stypes.ml");
  ("Typedecl", "typing/typedecl.ml");

  (* Comp *)
  ("Lambda", "bytecomp/lambda.ml");

  (* Typing *)
  ("Typeopt", "typing/typeopt.ml");
  ("Typecore", "typing/typecore.ml");
  ("Typeclass", "typing/typeclass.ml");
  ("Typemod", "typing/typemod.ml");

  (* Comp *)
  ("Cmo_format", "bytecomp/cmo_format.mli");
  ("Printlambda", "bytecomp/printlambda.ml");
  ("Semantics_of_primitives", "bytecomp/semantics_of_primitives.ml");
  ("Switch", "bytecomp/switch.ml");
  ("Matching", "bytecomp/matching.ml");
  ("Translobj", "bytecomp/translobj.ml");
  ("Translattribute", "bytecomp/translattribute.ml");
  ("Translprim", "bytecomp/translprim.ml");
  ("Translcore", "bytecomp/translcore.ml");
  ("Translclass", "bytecomp/translclass.ml");
  ("Translmod", "bytecomp/translmod.ml");
  ("Simplif", "bytecomp/simplif.ml");
  ("Runtimedef", "bytecomp/runtimedef.ml");
  ("Meta", "bytecomp/meta.ml");
  ("Opcodes", "bytecomp/opcodes.ml");
  ("Bytesections", "bytecomp/bytesections.ml");
  ("Dll", "bytecomp/dll.ml");
  ("Symtable", "bytecomp/symtable.ml");
  ("Pparse", "driver/pparse.ml");
  ("Main_args", "driver/main_args.ml");
  ("Compenv", "driver/compenv.ml");
  ("Compmisc", "driver/compmisc.ml");
  ("Compdynlink", "driver/compdynlink.mlno");
  ("Compplugin", "driver/compplugin.ml");
  ("Makedepend", "driver/makedepend.ml");

  (* Bytecomp *)
  ("Instruct", "bytecomp/instruct.ml");
  ("Bytegen", "bytecomp/bytegen.ml");
  ("Printinstr", "bytecomp/printinstr.ml");
  ("Emitcode", "bytecomp/emitcode.ml");
  ("Bytelink", "bytecomp/bytelink.ml");
  ("Bytelibrarian", "bytecomp/bytelibrarian.ml");
  ("Bytepackager", "bytecomp/bytepackager.ml");
  ("Errors", "driver/errors.ml");
  ("Compile", "driver/compile.ml");

  (* Bytestart *)
  ("Main", "driver/main.ml");
]

let compiler_path = (*"/home/nathanael/.opam/4.07.0/lib/ocaml/compiler-libs"*) "/home/nathanael/Projects/ocaml"
let compiler_modules = List.map (fun np -> let (n, p) = np in (n, compiler_path ^ "/" ^ p)) compiler_modules

let _ =
  load_modules init_env compiler_modules

