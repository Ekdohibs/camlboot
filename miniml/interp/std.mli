external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( ~- ) : int -> int = "%negint"
external ( mod ) : int -> int -> int = "%modint"
external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"
external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"
external compare : 'a -> 'a -> int = "caml_compare"
external ( = ) : 'a -> 'a -> bool = "caml_equal"
external ( <> ) : 'a -> 'a -> bool = "caml_notequal"
external ( <= ) : 'a -> 'a -> bool = "caml_lessequal"
external ( < ) : 'a -> 'a -> bool = "caml_lessthan"
external ( >= ) : 'a -> 'a -> bool = "caml_greaterequal"
external ( > ) : 'a -> 'a -> bool = "caml_greaterthan"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"
external raise : exn -> 'a = "%raise"
external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"
val invalid_arg : string -> 'a
val failwith : string -> 'a
val ignore : 'a -> unit
val succ : int -> int
val pred : int -> int
val max_int : int
val min_int : int
val min : 'a -> 'a -> 'a
val max : 'a -> 'a -> 'a
val abs : int -> int
type 'a ref = { mutable contents : 'a; }
type ('a, 'b) result = Ok of 'a | Error of 'b
type 'a list = [] | (::) of 'a * 'a list
exception Exit
external __array_get : 'a array -> int -> 'a = "caml_array_get"
external __array_set : 'a array -> int -> 'a -> unit = "caml_array_set"
external __string_get : string -> int -> char = "caml_string_get"
external __string_set : string -> int -> char -> unit = "caml_bytes_set"
module Obj :
  sig
    type t
    external obj : t -> 'a = "%identity"
    external repr : 'a -> t = "%identity"
    external magic : 'a -> 'b = "%identity"
    external is_block : t -> bool = "caml_obj_is_block"
    external new_block : int -> int -> t = "caml_obj_block"
    external tag : t -> int = "caml_obj_tag"
    external set_tag : t -> int -> unit = "caml_obj_set_tag"
    external size : t -> int = "%obj_size"
    external field : t -> int -> t = "%obj_field"
    external set_field : t -> int -> t -> unit = "%obj_set_field"
    external is_int : t -> bool = "%obj_is_int"
    val lazy_tag : int
    val forward_tag : int
    val string_tag : int
  end
val int_of_char : char -> int
val char_of_int : int -> char
external string_length : string -> int = "%string_length"
external bytes_length : bytes -> int = "%bytes_length"
external bytes_create : int -> bytes = "caml_create_bytes"
external string_blit : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string"
external bytes_blit : bytes -> int -> bytes -> int -> int -> unit
  = "caml_blit_bytes"
external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"
val ( ^ ) : string -> string -> string
module Uchar :
  sig
    type t
    val unsafe_of_int : int -> t
    val to_int : t -> int
    val is_valid : int -> bool
  end
val ( @ ) : 'a list -> 'a list -> 'a list
val ref : 'a -> 'a ref
val ( ! ) : 'a ref -> 'a
val ( := ) : 'a ref -> 'a -> unit
val incr : int ref -> unit
val decr : int ref -> unit
external not : bool -> bool = "%boolnot"
external __array_make : int -> 'a -> 'a array = "caml_make_vect"
val __atom0 : int array
external int_of_string : string -> int = "caml_int_of_string"
external format_int : string -> int -> string = "caml_format_int"
val string_of_int : int -> string
type in_channel
type out_channel
external unsafe_output : out_channel -> bytes -> int -> int -> unit
  = "caml_ml_output_bytes"
val output : out_channel -> bytes -> int -> int -> unit
external open_descriptor_out : int -> out_channel
  = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel
  = "caml_ml_open_descriptor_in"
type open_flag =
    Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock
external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"
external set_in_channel_name : in_channel -> string -> unit
  = "caml_ml_set_channel_name"
val open_in_gen : open_flag list -> int -> string -> in_channel
val open_in : string -> in_channel
val open_in_bin : string -> in_channel
external set_out_channel_name : out_channel -> string -> unit
  = "caml_ml_set_channel_name"
val open_out_gen : open_flag list -> int -> string -> out_channel
val open_out : string -> out_channel
val open_out_bin : string -> out_channel
external close_in : in_channel -> unit = "caml_ml_close_channel"
val close_in_noerr : in_channel -> unit
external flush : out_channel -> unit = "caml_ml_flush"
external close_out_channel : out_channel -> unit = "caml_ml_close_channel"
val close_out : out_channel -> unit
val stdin : in_channel
val stdout : out_channel
val stderr : out_channel
external unsafe_output_string : out_channel -> string -> int -> int -> unit
  = "caml_ml_output"
external output_binary_int : out_channel -> int -> unit
  = "caml_ml_output_int"
external output_char : out_channel -> char -> unit = "caml_ml_output_char"
external pos_out : out_channel -> int = "caml_ml_pos_out"
external seek_out : out_channel -> int -> unit = "caml_ml_seek_out"
external input_binary_int : in_channel -> int = "caml_ml_input_int"
external input_char : in_channel -> char = "caml_ml_input_char"
external input_value : in_channel -> 'a = "caml_input_value"
external seek_in : in_channel -> int -> unit = "caml_ml_seek_in"
external pos_in : in_channel -> int = "caml_ml_pos_in"
val output_string : out_channel -> string -> unit
val output_bytes : out_channel -> bytes -> unit
val print_char : char -> unit
val print_string : string -> unit
val print_bytes : bytes -> unit
val print_newline : unit -> unit
val print_endline : string -> unit
val print_err : string -> unit
val prerr_string : string -> unit
val prerr_int : int -> unit
val prerr_newline : unit -> unit
external unsafe_input : in_channel -> bytes -> int -> int -> int
  = "caml_ml_input"
val input : in_channel -> bytes -> int -> int -> int
val unsafe_really_input : in_channel -> bytes -> int -> int -> unit
val really_input : in_channel -> bytes -> int -> int -> unit
val really_input_string : in_channel -> int -> string
module Sys :
  sig
    type backend_type = Native | Bytecode | Other of string
    external getenv : string -> string = "caml_sys_getenv"
    val backend_type : backend_type
    val getenv_opt : string -> string option
    val max_string_length : int
    val max_array_length : int
    val word_size : int
    external get_argv : unit -> string * string array = "caml_sys_get_argv"
    val eav : string * string array
    val executable_name : string
    val argv : string array
    val big_endian : bool
    external file_exists : string -> bool = "caml_sys_file_exists"
    external getcwd : unit -> string = "caml_sys_getcwd"
    external rename : string -> string -> unit = "caml_sys_rename"
    external remove : string -> unit = "caml_sys_remove"
    external readdir : string -> string array = "caml_sys_read_directory"
    val os_type : string
    val ocaml_version : string
  end
external float_of_string : string -> float = "caml_float_of_string"
external sys_exit : int -> 'a = "caml_sys_exit"
external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed]
  [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1" [@@unboxed]
  [@@noalloc]
external acos : float -> float = "caml_acos_float" "acos" [@@unboxed]
  [@@noalloc]
external asin : float -> float = "caml_asin_float" "asin" [@@unboxed]
  [@@noalloc]
external atan : float -> float = "caml_atan_float" "atan" [@@unboxed]
  [@@noalloc]
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]
external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
  [@@unboxed] [@@noalloc]
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed]
  [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh" [@@unboxed]
  [@@noalloc]
external log : float -> float = "caml_log_float" "log" [@@unboxed]
  [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10" [@@unboxed]
  [@@noalloc]
external log1p : float -> float = "caml_log1p_float" "caml_log1p" [@@unboxed]
  [@@noalloc]
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed]
  [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh" [@@unboxed]
  [@@noalloc]
external sqrt : float -> float = "caml_sqrt_float" "sqrt" [@@unboxed]
  [@@noalloc]
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed]
  [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh" [@@unboxed]
  [@@noalloc]
external ceil : float -> float = "caml_ceil_float" "ceil" [@@unboxed]
  [@@noalloc]
external floor : float -> float = "caml_floor_float" "floor" [@@unboxed]
  [@@noalloc]
external abs_float : float -> float = "%absfloat"
external copysign : float -> float -> float = "caml_copysign_float"
  "caml_copysign" [@@unboxed] [@@noalloc]
external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]
external frexp : float -> float * int = "caml_frexp_float"
external ldexp :
  (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed])
  = "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
  "caml_int64_float_of_bits_unboxed" [@@unboxed] [@@noalloc]
external format_float : string -> float -> string = "caml_format_float"
val valid_float_lexem : string -> string
val string_of_float : float -> string
module Pervasives : sig val compare : 'a -> 'a -> int end
val string_of_bool : bool -> string
val bool_of_string : string -> bool
val bool_of_string_opt : string -> bool option
external out_channels_list : unit -> out_channel list
  = "caml_ml_out_channels_list"
val flush_all : unit -> unit
val exit_function : (unit -> unit) ref
val at_exit : (unit -> unit) -> unit
val __atexit : unit -> unit
val exit : int -> 'a
