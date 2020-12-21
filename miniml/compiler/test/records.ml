let () = print_endline "Records:"
let () = print "simple: "

type t = { a : int ; b : int }

let () =
  let u = { a = 5 ; b = 7 } in
  print_int u.a; print_int u.b

let () =
  let u = { b = 5 ; a = 7 } in
  print_int u.a; print_int u.b

let () = print_newline ()
let () = print "with: "

let () =
  let u = { a = 5 ; b = 7 } in
  let v = { u with a = 42 } in
  let w = { u with b = 16 } in
  print_int u.a; print_int u.b;
  print_int v.a; print_int v.b;
  print_int w.a; print_int w.b

let () = print_newline ()
let () = print "record field punning: "

let () =
  let u = let a, b = 1, 2 in { a; b } in
  match u with
  | {a; b} -> print_int a; print_int b

let () = print_newline ()
let () = print "inline records: "

type t =
  | A of { x : int; y : int; u : int }
  | B of { z : int }

let print_t = function
  | A { x; u } -> print_int x; print_int u (* note: field y is ignored *)
  | B { z = name } -> print_int name

let rec loop = function
| [] -> ()
| t :: ts -> print_t t; loop ts

let () = let z = 3 in loop [
  A { x = 1; y = 0; u = 2 };
  B { z; }
]

let () = print_newline ()
