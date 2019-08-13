open Data
open Runtime_lib

let wrap_in_channel ic = ptr @@ InChannel ic

let unwrap_in_channel = onptr @@ function
  | InChannel ic -> ic
  | _ -> assert false

let wrap_out_channel oc = ptr @@ OutChannel oc

let unwrap_out_channel = onptr @@ function
  | OutChannel oc -> oc
  | _ -> assert false

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

let unwrap_open_flag = onptr @@ function
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
  | x :: l ->
     ptr @@ Constructor ("::", 0,
                         Some (ptr @@ Tuple [ wrapf x; wrap_list wrapf l ]))

let rec unwrap_list unwrapf = onptr @@ function
  | Constructor ("[]", _, None) -> []
  | Constructor ("::", _, Some arg) ->
    begin match Ptr.get arg with
      | Tuple [ x; l ] ->
         unwrapf x :: unwrap_list unwrapf l
      | _ -> assert false
    end
  | _ -> assert false

let unwrap_marshal_flag = onptr @@ function
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
external close_desc: int -> unit = "caml_sys_close"

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

let rec seeded_hash_param meaningful total seed = onptr @@ function
  | Int n -> Hashtbl.seeded_hash_param meaningful total seed n
  | Int32 n -> Hashtbl.seeded_hash_param meaningful total seed n
  | Int64 n -> Hashtbl.seeded_hash_param meaningful total seed n
  | Nativeint n -> Hashtbl.seeded_hash_param meaningful total seed n
  | Float f -> Hashtbl.seeded_hash_param meaningful total seed f
  | Tuple _l -> 0
  | String s ->
    Hashtbl.seeded_hash_param meaningful total seed (Bytes.to_string s)
  | Constructor (c, _, _v) -> Hashtbl.seeded_hash_param meaningful total seed c
  | Array _a -> 0
  | Record _r -> 0
  | Fexpr _ | Fun _ | Function _ | InChannel _ | OutChannel _ | Prim _ | Lz _
  | ModVal _ | Fun_with_extra_args _ ->
    assert false

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
