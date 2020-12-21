let () = print_endline "Records:"
let () = print_string "simple: "

type t = { a : int ; b : int }

let () =
  let u = { a = 5 ; b = 7 } in
  show_int u.a; show_int u.b

let () =
  let u = { b = 5 ; a = 7 } in
  show_int u.a; show_int u.b

let () = print_newline ()
let () = print_string "with: "

let () =
  let u = { a = 5 ; b = 7 } in
  let v = { u with a = 42 } in
  let w = { u with b = 16 } in
  show_int u.a; show_int u.b;
  show_int v.a; show_int v.b;
  show_int w.a; show_int w.b

let () = print_newline ()
let () = print_string "record field punning: "

let () =
  let u = let a, b = 1, 2 in { a; b } in
  match u with
  | {a; b} -> show_int a; show_int b

let () = print_newline ()
let () = print_string "inline records: "

type t =
  | A of { x : int; y : int; u : int }
  | B of { z : int }

let print_t = function
  | A { x; u } -> show_int x; show_int u (* note: field y is ignored *)
  | B { z = name } -> show_int name

let rec loop = function
| [] -> ()
| t :: ts -> print_t t; loop ts

let () = let z = 3 in loop [
  A { x = 1; y = 0; u = 2 };
  B { z; }
]

let () = print_newline ()
