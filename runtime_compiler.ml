open Data
open Runtime_lib
open Runtime_base

let wrap_array_id a = Array a

let unwrap_array_id = function
  | Array a -> a
  | _ -> assert false

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

let apply_ref =
  ref
    (fun _ _ -> assert false
      : value -> (Asttypes.arg_label * value) list -> value)

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
      | Constructor (_c, d, None) -> Obj.repr d
      | Constructor (_c, d, Some arg) ->
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

let parse_engine_prim =
  prim4
    parse_engine_wrapper
    wrap_exn
    id
    id
    unwrap_parser_input
    id
    wrap_parser_output

let lex_engine_prim =
  prim3 lex_engine_wrapper wrap_exn id unwrap_int id wrap_int

let new_lex_engine_prim =
  prim3 new_lex_engine_wrapper wrap_exn id unwrap_int id wrap_int
