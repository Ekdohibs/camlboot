open Data

let wrap_int n = ptr @@ Int n
let unwrap_int  = onptr @@ function
  | Int n -> n
  | _ -> assert false

let wrap_int32 n = ptr @@ Int32 n
let unwrap_int32 = onptr @@ function
  | Int32 n -> n
  | _ -> assert false

let wrap_int64 n = ptr @@ Int64 n
let unwrap_int64 = onptr @@ function
  | Int64 n -> n
  | _ -> assert false

let wrap_nativeint n = ptr @@ Nativeint n
let unwrap_nativeint = onptr @@ function
  | Nativeint n -> n
  | v ->
     Format.eprintf "unwrap_nativeint %a@."
       pp_print_value (Ptr.create v);
     assert false

let wrap_float f = ptr @@ Float f
let unwrap_float = onptr @@ function
  | Float f -> f
  | _ -> assert false

let unwrap_bool = is_true

let wrap_bool b = ptr @@
  if b then Constructor ("true", 1, None) else Constructor ("false", 0, None)

let wrap_unit () = unit

let unwrap_unit = onptr @@ function
  | Constructor ("()", 0, None) -> ()
  | _ -> assert false

let wrap_bytes s = ptr @@ String s

let unwrap_bytes = onptr @@ function
  | String s -> s
  | _ -> assert false

let wrap_string s = ptr @@ String (Bytes.of_string s)

let unwrap_string = onptr @@ function
  | String s -> Bytes.to_string s
  | _ -> assert false

let wrap_string_unsafe s = ptr @@ String (Bytes.unsafe_of_string s)

let unwrap_string_unsafe = onptr @@ function
  | String s -> Bytes.unsafe_to_string s
  | _ -> assert false

let wrap_char c = ptr @@ Int (int_of_char c)

let unwrap_char = onptr @@ function
  | Int n -> char_of_int (n land 255)
  | _ -> assert false

let wrap_array wrapf a = ptr @@ Array (Array.map wrapf a)

let unwrap_array unwrapf = onptr @@ function
  | Array a -> Array.map unwrapf a
  | _ -> assert false

let declare_builtin_constructor name d env =
  Envir.env_set_constr name d env

let declare_exn name env =
  let d = next_exn_id () in
  declare_builtin_constructor name d env

let initial_env =
  Envir.empty_env
  |> declare_exn "Not_found"
  |> declare_exn "Exit"
  |> declare_exn "Invalid_argument"
  |> declare_exn "Failure"
  |> declare_exn "Match_failure"
  |> declare_exn "Assert_failure"
  |> declare_exn "Sys_blocked_io"
  |> declare_exn "Sys_error"
  |> declare_exn "End_of_file"
  |> declare_exn "Division_by_zero"
  |> declare_exn "Undefined_recursive_module"
  |> declare_builtin_constructor "false" 0
  |> declare_builtin_constructor "true" 1
  |> declare_builtin_constructor "None" 0
  |> declare_builtin_constructor "Some" 0
  |> declare_builtin_constructor "[]" 0
  |> declare_builtin_constructor "::" 0
  |> declare_builtin_constructor "()" 0

let not_found_exn = Runtime_lib.exn0 initial_env "Not_found"

let exit_exn = Runtime_lib.exn0 initial_env "Exit"

let invalid_argument_exn =
  Runtime_lib.exn1 initial_env "Invalid_argument" wrap_string

let failure_exn = Runtime_lib.exn1 initial_env "Failure" wrap_string

let match_failure_exn =
  Runtime_lib.exn3 initial_env "Match_failure" wrap_string wrap_int wrap_int

let assert_failure_exn =
  Runtime_lib.exn3 initial_env "Assert_failure" wrap_string wrap_int wrap_int

let sys_blocked_io_exn = Runtime_lib.exn0 initial_env "Sys_blocked_io"

let sys_error_exn = Runtime_lib.exn1 initial_env "Sys_error" wrap_string

let end_of_file_exn = Runtime_lib.exn0 initial_env "End_of_file"

let division_by_zero_exn = Runtime_lib.exn0 initial_env "Division_by_zero"

let undefined_recursive_module_exn =
  Runtime_lib.exn3
    initial_env
    "Undefined_recursive_module"
    wrap_string
    wrap_int
    wrap_int

let wrap_exn = function
  | Not_found -> Some not_found_exn
  | Exit -> Some exit_exn
  | Invalid_argument s -> Some (invalid_argument_exn s)
  | Failure s -> Some (failure_exn s)
  | Match_failure (s, i, j) -> Some (match_failure_exn s i j)
  | Assert_failure (s, i, j) -> Some (assert_failure_exn s i j)
  | Sys_blocked_io -> Some sys_blocked_io_exn
  | Sys_error s -> Some (sys_error_exn s)
  | End_of_file -> Some end_of_file_exn
  | Division_by_zero -> Some division_by_zero_exn
  | Undefined_recursive_module (s, i, j) ->
    Some (undefined_recursive_module_exn s i j)
  | _ -> None
