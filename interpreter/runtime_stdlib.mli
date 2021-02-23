open Data

val wrap_in_channel : in_channel -> value
val unwrap_in_channel : value -> in_channel
val wrap_out_channel : out_channel -> value
val unwrap_out_channel : value -> out_channel
val wrap_open_flag : open_flag -> value
val unwrap_open_flag : value -> open_flag
val wrap_list : ('a -> value) -> 'a list -> value
val unwrap_list : (value -> 'a) -> value -> 'a list
val unwrap_marshal_flag : value -> Marshal.extern_flags

external open_descriptor_out : int -> out_channel
  = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel
  = "caml_ml_open_descriptor_in"
external open_desc : string -> open_flag list -> int -> int
  = "caml_sys_open"
external close_desc : int -> unit = "caml_sys_close"
external set_out_channel_name : out_channel -> string -> unit
  = "caml_ml_set_channel_name"
external out_channels_list : unit -> out_channel list
  = "caml_ml_out_channels_list"
external unsafe_output : out_channel -> bytes -> int -> int -> unit
  = "caml_ml_output_bytes"
external unsafe_output_string :
  out_channel -> string -> int -> int -> unit = "caml_ml_output"
external set_in_channel_name : in_channel -> string -> unit
  = "caml_ml_set_channel_name"
external unsafe_input : in_channel -> bytes -> int -> int -> int
  = "caml_ml_input"
external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"
external random_seed : unit -> int array = "caml_sys_random_seed"
val seeded_hash_param : int -> int -> int -> value -> int
external digest_unsafe_string : string -> int -> int -> string
  = "caml_md5_string"
external marshal_to_channel :
  out_channel -> 'a -> unit list -> unit = "caml_output_value"
external caml_output_value_to_string :
  'a -> Marshal.extern_flags list -> string
  = "caml_output_value_to_string"
external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external input_scan_line : in_channel -> int
  = "caml_ml_input_scan_line"
external caml_int32_format : string -> int32 -> string = "caml_int32_format"
external caml_int64_format : string -> int64 -> string = "caml_int64_format"
external caml_nativeint_format : string -> nativeint -> string
  = "caml_nativeint_format"
external caml_sys_system_command : string -> int = "caml_sys_system_command"
