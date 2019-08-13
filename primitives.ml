open Data
open Envir
open Runtime_lib
open Runtime_base
open Runtime_stdlib
open Runtime_compiler

let exp_of_desc loc desc =
  Parsetree.{ pexp_desc = desc; pexp_loc = loc; pexp_attributes = [] }

let seq_or loc = function
  | [ (_, arg1); (_, arg2) ] ->
    let open Parsetree in
    let expr_true =
      Pexp_construct ({ txt = Longident.Lident "true"; loc }, None)
    in
    Some
      (exp_of_desc
         loc
         (Pexp_ifthenelse (arg1, exp_of_desc loc expr_true, Some arg2)))
  | _ -> None

let seq_and loc = function
  | [ (_, arg1); (_, arg2) ] ->
    let open Parsetree in
    let expr_false =
      Pexp_construct ({ txt = Longident.Lident "false"; loc }, None)
    in
    Some
      (exp_of_desc
         loc
         (Pexp_ifthenelse (arg1, arg2, Some (exp_of_desc loc expr_false))))
  | _ -> None

let apply loc = function
  | [ (_, f); (_, x) ] ->
    let open Parsetree in
    Some (exp_of_desc loc (Pexp_apply (f, [ (Nolabel, x) ])))
  | _ -> None

let rev_apply loc = function
  | [ (_, x); (_, f) ] ->
    let open Parsetree in
    Some (exp_of_desc loc (Pexp_apply (f, [ (Nolabel, x) ])))
  | _ -> None

external reraise : exn -> 'a = "%reraise"
external raise_notrace : exn -> 'a = "%raise_notrace"

let prims =
  let prim1 f = prim1 f Runtime_base.wrap_exn in
  let prim2 f = prim2 f Runtime_base.wrap_exn in
  let prim3 f = prim3 f Runtime_base.wrap_exn in
  let prim4 f = prim4 f Runtime_base.wrap_exn in
  let prim5 f = prim5 f Runtime_base.wrap_exn in
  [ ("%apply", ptr @@ Fexpr apply);
    ("%revapply", ptr @@ Fexpr rev_apply);
    ("%raise", ptr @@ Prim (fun v -> raise (InternalException v)));
    ("%reraise", ptr @@ Prim (fun v -> reraise (InternalException v)));
    ("%raise_notrace", ptr @@ Prim (fun v -> raise_notrace (InternalException v)));
    ("%sequand", ptr @@ Fexpr seq_and);
    ("%sequor", ptr @@ Fexpr seq_or);
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
    ("%identity", ptr @@ Prim (fun x -> x));
    ("caml_register_named_value",
     ptr @@ Prim (fun _ -> ptr @@ Prim (fun _ -> unit)));
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
          match Ptr.get v with
          | InChannel ic -> set_in_channel_name ic s
          | OutChannel oc -> set_out_channel_name oc s
          | _ -> assert false)
        id
        unwrap_string
        wrap_unit );
    ( "caml_ml_close_channel",
      prim1
        (onptr @@ function
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
    ("%makemutable",
     ptr @@ Prim (fun v -> ptr @@ Record (SMap.singleton "contents" (ref v))));
    ( "%field0",
      ptr @@ Prim
        (onptr @@ function
        | Record r -> !(SMap.find "contents" r)
        | Tuple l -> List.hd l
        | _ -> assert false) );
    ( "%field1",
      ptr @@ Prim
        (onptr @@ function
        | Tuple l -> List.hd (List.tl l)
        | _ -> assert false) );
    ( "%setfield0",
      ptr @@ Prim
        (onptr @@ function
        | Record r ->
          ptr @@ Prim
            (fun v ->
              SMap.find "contents" r := v;
              unit)
        | _ -> assert false) );
    ( "%incr",
      ptr @@ Prim
        (onptr @@ function
        | Record r ->
          let z = SMap.find "contents" r in
          z := wrap_int (unwrap_int !z + 1);
          unit
        | _ -> assert false) );
    ( "%decr",
      ptr @@ Prim
        (onptr @@ function
        | Record r ->
          let z = SMap.find "contents" r in
          z := wrap_int (unwrap_int !z - 1);
          unit
        | _ -> assert false) );
    ("%ignore", ptr @@ Prim (fun _ -> unit));
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
      ptr @@ Prim
        (fun _ -> ptr @@
          Tuple [ wrap_string "";
                  ptr @@ Array (Array.map wrap_string Sys.argv) ]) );
    ( "caml_sys_get_config",
      ptr @@ Prim
      (fun _ -> ptr @@ Tuple [ wrap_string "Unix"; wrap_int 0; wrap_bool true ]) );
    ("%big_endian", ptr @@ Prim (fun _ -> wrap_bool Sys.big_endian));
    ("%word_size", ptr @@ Prim (fun _ -> ptr @@ Int 64));
    ("%int_size", ptr @@ Prim (fun _ -> ptr @@ Int 64));
    ("%max_wosize", ptr @@ Prim (fun _ -> ptr @@ Int 1000000));
    ("%ostype_unix", ptr @@ Prim (fun _ -> wrap_bool false));
    ("%ostype_win32", ptr @@ Prim (fun _ -> wrap_bool false));
    ("%ostype_cygwin", ptr @@ Prim (fun _ -> wrap_bool false));
    ( "%backend_type", ptr @@
      Prim (fun _ ->
          ptr @@ Constructor ("Other", 0, Some (wrap_string "Interpreter")))
    );
    ("caml_sys_getenv", prim1 Sys.getenv unwrap_string wrap_string);
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
    ("%bytes_to_string", ptr @@ Prim (fun v -> v));
    ("%bytes_of_string", ptr @@ Prim (fun v -> v));
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
      ptr @@ Prim
        (onptr @@ function
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
        (fun n -> Array.make n (ptr @@ Constructor ("None", 0, None)))
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
        (fun a n -> Ptr.get a.(n) <> Constructor ("None", 0, None))
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
    (* Spacetime *)
    ( "caml_spacetime_enabled",
      let module Prim = struct
        external spacetime_enabled : unit -> bool = "caml_spacetime_enabled"
          [@@noalloc]
      end
      in
      prim1 Prim.spacetime_enabled unwrap_unit wrap_bool );
    (* Gc *)
    ("caml_gc_quick_stat", prim1 Gc.quick_stat unwrap_unit wrap_gc_stat);
    (* utils/profile.ml *)
    ( "caml_sys_time_include_children",
      let module Prim = struct
        external time_include_children
          :  bool ->
          float
          = "caml_sys_time_include_children"
      end
      in
      prim1 Prim.time_include_children unwrap_bool wrap_float );
    (* utils/misc.ml *)
    ( "caml_sys_isatty",
      let module Prim = struct
        external isatty : out_channel -> bool = "caml_sys_isatty"
      end
      in
      prim1 Prim.isatty unwrap_out_channel wrap_bool );
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
        (onptr @@ function
          | Array a -> Array.length a + 2
          | _ -> 4)
        id
        wrap_int );
    ( "caml_obj_block",
      prim2
        (fun tag size ->
          let block = ptr @@ Array (Array.init size (fun _ -> ptr @@ Int 0)) in
          ptr @@ Constructor ("", tag, Some block))
        unwrap_int
        unwrap_int
        id );
    ( "%obj_set_field",
      prim3 (fun data idx v ->
          let err () =
            Format.eprintf "obj_set_field (%a).(%d) <- (%a)@."
              pp_print_value data
              idx
              pp_print_value v in
          match Ptr.get data with
            | Array arr -> arr.(idx) <- v
            | Constructor(_, _, Some arg) ->
               begin match Ptr.get arg with
                 | Array arr -> arr.(idx) <- v
                 | _ -> err (); assert false
               end
            | _ -> err (); assert false
        )
        id
        unwrap_int
        id
        wrap_unit );
  ]

let prims =
  List.fold_left (fun env (name, v) -> SMap.add name v env) SMap.empty prims

let () = Runtime_compiler.apply_ref := Eval.apply prims
