let () = print_endline "Local exits"

let () = print_string "simple: "

(* exit not taken *)
let () = show_int (let%exit ex = 42 in 0)

(* constant exit *)
let () = show_int (let%exit ex = 1 in [%exit] ex)

(* one-parameter exit *)
let () = show_int (let%exit ex n = 2 * n in [%exit] ex 1)

(* two-parameters exit *)
let () =
  let%exit ex m n = show_int (m * m + n) in
  [%exit] ex 1 (1 + 1)

let () = print_newline ()
let () = print_string "multi-exits: "

(* two exits *)
let () =
  show_int (
    let%exit ex1 = 0
    and ex2 = 7
    in
    ([%exit] ex1) + 8
    (* note: exits interrupt the computation, so the + 8 is discarded *)
  )

(* two exits of different arity, max arity taken *)
let () =
  show_int (
    let%exit ex1 n = n - 3
    and ex2 = 3
    in
    ([%exit] ex1 4) + 8
  )

(* two exits of different arity, below-max arity taken *)
let () =
  show_int (
    let%exit ex1 n = n + 4
    and ex2 = 2
    in
    ([%exit] ex2) + 8
  )

let () = print_newline ()
let () = print_string "exceptions: "

exception Foo of int

(* exiting within an exception handler *)
let () =
  try
    let%exit ex n = raise (Foo n) in
    [%exit] ex 0
  with Foo n -> show_int n

(* exiting across an exception handler *)
let () =
  show_int (
    let%exit ex n = n + 2 in
    try
      [%exit] ex (-1)
    with Foo n -> show_int n
  )

(* exiting across two exception handlers *)
let () =
  try
    let%exit ex n = raise (Foo n) in
    try
      try
        [%exit] ex 2
      with Invalid_argument _ -> ()
    with Foo n -> show_int (2 * n)
  with Foo n -> show_int n

(* exiting from an exception handler which is not at the top of the stack;
   we need another exception handler below, which is used, to observe
   scenarios where POPTRAP would be used at the wrong place and corrupt
   caml_trapsp. *)
let () =
  try
    let%exit ex n = raise (Foo (n + 2)) in
    try
      let v = 1+0 in
      [%exit] ex v
    with Foo n -> show_int (2 * n)
  with Foo n -> show_int n

let () = print_newline ()
