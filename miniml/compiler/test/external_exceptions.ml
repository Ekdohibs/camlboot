let () = print_endline "Externally raised exceptions:"

external obj_tag : Obj.t -> int = "caml_obj_tag"
external obj_size : Obj.t -> int = "%79"
external obj_field : Obj.t -> int -> Obj.t = "%80"

let rec print_obj x =
  let t = obj_tag x in
  if t = 1000 then print_string (format_int "%d" x)
  else if t = 1001 then print_string "<out of heap>"
  else if t = 1002 then print_string "<unaligned>"
  else if t = 252 then (print_string "\""; print_string x; print_string "\"")
  else (print_string (format_int "%d" t); print_string "["; print_obj_fields x 0; print_string "]")

and print_obj_fields x i =
  if i = obj_size x then ()
  else if i = obj_size x - 1 then print_obj (obj_field x i)
  else (print_obj (obj_field x i); print_string " "; print_obj_fields x (i + 1))

let print_exn e =
  match e with
  | Out_of_memory -> print_string "Out_of_memory"
  | Sys_error s -> print_string "Sys_error \""; print_string s; print_string "\""
  | Failure s -> print_string "Failure \""; print_string s; print_string "\""
  | Invalid_argument s -> print_string "Invalid_argument \""; print_string s; print_string "\""
  | End_of_file -> print_string "End_of_file"
  | Division_by_zero -> print_string "Division_by_zero"
  | Not_found -> print_string "Not_found"
  | Match_failure _ -> print_string "Match_failure _"
  | Stack_overflow -> print_string "Stack overflow"
  | Sys_blocked_io -> print_string "Sys_blocked_io"
  | Assert_failure _ -> print_string "Assert_failure _"
  | Undefined_recursive_module _ -> print_string "Undefined_recursive_module _"
  | _ -> print_string "<unknown>"

let run_and_print_exn f =
  try f (); print_string "no exception\n" with e -> (print_obj e; print_string " "; print_exn e; print_string "\n")

external int_of_string : string -> int = "caml_int_of_string"
external sys_getenv : string -> string = "caml_sys_getenv"

let () = run_and_print_exn (fun () -> (fun x -> ()) = (fun x -> ()))
let () = run_and_print_exn (fun () -> int_of_string "fqsq")
let () = run_and_print_exn (fun () -> sys_getenv "fqsq")
let rec stack_overflow () = 1 + stack_overflow ()
let () = run_and_print_exn stack_overflow
let () = run_and_print_exn (fun () -> 1 / 0)

let () = print_newline ()
