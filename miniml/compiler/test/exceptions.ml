let () = print_endline "Exceptions:"

exception E1
exception E2 of int
exception E3
exception E4 of int
exception E5 of { x : int }

let show_exn e =
  match e with
  | E1 -> print_string "E1"
  | E2 i -> print_string "(E2"; show_int i; print_string ")"
  | _ -> print_string "<unknown>"

let () =
  try raise E1 with
  | E1 -> print_string " ok"
  | _ -> print_string " ko"

let () =
  try raise (E2 7) with
    (* note: no leading bar *)
    E2 x -> if x = 7 then print_string " ok" else print_string " ko"
  | _ -> print_string " ko"

let () = print_string (try " ok" with _ -> " ko")

let () = try (try raise E1 with E2 _ -> print_string " ko") with E1 -> print_string " ok" | _ -> print_string " ko"
let () = try (try raise (E2 7) with E1 -> print_string " ko") with E2 x -> if x = 7 then print_string " ok" else print_string " ko" | _ -> print_string " ko"
let () = try (try raise E3 with E1 -> print_string " ko" | E2 _ -> print_string " ko") with _ -> print_string " ok"
let () = try (try raise (E4 7) with E1 -> print_string " ko" | E2 _ -> print_string " ko") with _ -> print_string " ok"

let () = try (try raise E1 with E1 -> print_string " ok") with _ -> print_string " ko"
let () = try (try raise (E2 7) with E2 x -> if x = 7 then print_string " ok" else print_string " ko") with _ -> print_string " ko"

let () = print_string " "; show_exn E1
let () = print_string " "; show_exn (E2 7)
let () = print_string " "; show_exn E3
let () = print_string " "; show_exn (E4 7)
let () = try raise (E5 { x = 8 }) with E5 { x } -> show_int x

let () = print_newline ()
