open Data

val wrap_array_id : value array -> value
val unwrap_array_id : value -> value array
val unwrap_position : value -> Lexing.position
val wrap_position : Lexing.position -> value
val wrap_gc_stat : Gc.stat -> value
type parser_env = {
  mutable s_stack : int array;
  mutable v_stack : Obj.t array;
  mutable symb_start_stack : Lexing.position array;
  mutable symb_end_stack : Lexing.position array;
  mutable stacksize : int;
  mutable stackbase : int;
  mutable curr_char : int;
  mutable lval : Obj.t;
  mutable symb_start : Lexing.position;
  mutable symb_end : Lexing.position;
  mutable asp : int;
  mutable rule_len : int;
  mutable rule_number : int;
  mutable sp : int;
  mutable state : int;
  mutable errflag : int;
}
type parse_tables = {
  actions : (parser_env -> Obj.t) array;
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
  names_block : string;
}
type parser_input =
    Start
  | Token_read
  | Stacks_grown_1
  | Stacks_grown_2
  | Semantic_action_computed
  | Error_detected
val unwrap_parser_input : value -> parser_input
type parser_output =
    Read_token
  | Raise_parse_error
  | Grow_stacks_1
  | Grow_stacks_2
  | Compute_semantic_action
  | Call_error_function
val wrap_parser_output : parser_output -> value
val unwrap_parser_env : value -> parser_env
val sync_parser_env : parser_env -> value -> unit
val apply_ref :
  (value -> (Asttypes.arg_label * value) list -> value)
  ref
val unwrap_parse_tables :
  value -> value -> parse_tables
external parse_engine :
  parse_tables -> parser_env -> parser_input -> Obj.t -> parser_output
  = "caml_parse_engine"
external lex_engine : Lexing.lex_tables -> int -> Lexing.lexbuf -> int
  = "caml_lex_engine"
external new_lex_engine : Lexing.lex_tables -> int -> Lexing.lexbuf -> int
  = "caml_new_lex_engine"
val parse_engine_wrapper :
  value ->
  value -> parser_input -> value -> parser_output
val unwrap_lexbuf : value -> Lexing.lexbuf
val sync_lexbuf : value -> Lexing.lexbuf -> unit
val unwrap_lex_tables : value -> Lexing.lex_tables
val lex_engine_wrapper :
  value -> int -> value -> int
val new_lex_engine_wrapper :
  value -> int -> value -> int
val parse_engine_prim : value
val lex_engine_prim : value
val new_lex_engine_prim : value
