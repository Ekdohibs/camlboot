let () = print_endline "Exceptions:"

exception E1
exception E2 of int
exception E3
exception E4 of int

let show_exn e =
  match e with
  | E1 -> print "E1"
  | E2 i -> print "(E2"; print_int i; print ")"
  | _ -> print "<unknown>"

let () =
  try raise E1 with
  | E1 -> print " ok"
  | _ -> print " ko"

let () =
  try raise (E2 7) with
    (* note: no leading bar *)
    E2 x -> if x = 7 then print " ok" else print " ko"
  | _ -> print " ko"

let () = print (try " ok" with _ -> " ko")

let () = try (try raise E1 with E2 _ -> print " ko") with E1 -> print " ok" | _ -> print " ko"
let () = try (try raise (E2 7) with E1 -> print " ko") with E2 x -> if x = 7 then print " ok" else print " ko" | _ -> print " ko"
let () = try (try raise E3 with E1 -> print " ko" | E2 _ -> print " ko") with _ -> print " ok"
let () = try (try raise (E4 7) with E1 -> print " ko" | E2 _ -> print " ko") with _ -> print " ok"

let () = try (try raise E1 with E1 -> print " ok") with _ -> print " ko"
let () = try (try raise (E2 7) with E2 x -> if x = 7 then print " ok" else print " ko") with _ -> print " ko"

let () = print " "; show_exn E1
let () = print " "; show_exn (E2 7)
let () = print " "; show_exn E3
let () = print " "; show_exn (E4 7)

let () = print_newline ()
