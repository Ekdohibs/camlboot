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
