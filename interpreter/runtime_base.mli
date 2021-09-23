open Data

val type_error : string -> Data.value_ -> 'a
val wrap_int : int -> value
val unwrap_int : value -> int
val wrap_int32 : int32 -> value
val unwrap_int32 : value -> int32
val wrap_int64 : int64 -> value
val unwrap_int64 : value -> int64
val wrap_nativeint : nativeint -> value
val unwrap_nativeint : value -> nativeint
val wrap_float : float -> value
val unwrap_float : value -> float
val unwrap_bool : value -> bool
val wrap_bool : bool -> value
val wrap_unit : unit -> value
val unwrap_unit : value -> unit
val wrap_bytes : bytes -> value
val unwrap_bytes : value -> bytes
val wrap_string : string -> value
val unwrap_string : value -> string
val wrap_string_unsafe : string -> value
val unwrap_string_unsafe : value -> string
val wrap_char : char -> value
val unwrap_char : value -> char
val wrap_array : ('a -> value) -> 'a array -> value
val unwrap_array : (value -> 'a) -> value -> 'a array
val declare_builtin_constructor :
  SMap.key -> int -> env -> env
val declare_exn : SMap.key -> env -> env
val initial_env : env
val not_found_exn : value
val exit_exn : value
val invalid_argument_exn : string -> value
val failure_exn : string -> value
val match_failure_exn : string -> int -> int -> value
val assert_failure_exn : string -> int -> int -> value
val sys_blocked_io_exn : value
val sys_error_exn : string -> value
val end_of_file_exn : value
val division_by_zero_exn : value
val undefined_recursive_module_exn :
  string -> int -> int -> value
val wrap_exn : exn -> value option
