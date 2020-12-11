let () = print_endline "Arguments:"

let f1 ~x ~y = print_int (x + 2 * y)
let () = f1 0 1
let () = f1 ~x:0 ~y:1
let () = f1 ~y:1 ~x:0

let f2 ?(x=1) ~y = print_int (x + 2 * y)
let () = f2 100
let () = f2 ~y:101 (* Note: this is different from ocaml *)
let () = f2 ?x:None ~y:102
let () = f2 ?x:(Some 0) ~y:103
let () = f2 ~x:0 ~y:104

let f3 () ?(x=1) (y1, y2) ~z = print_int x; print_int y1; print_int y2; print_int z
let () = f3 () (2, 3) ~z:4
let () = f3 () ~x:0 (1, 2) ~z:3

let () = print_newline ()
