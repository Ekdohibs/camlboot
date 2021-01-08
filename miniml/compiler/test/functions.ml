let _ = print_endline "Functions:"

let () = print_string "simple: "
(* let g x = let z = x * 2 in fun y -> z * 3 *)

let g x y = x - y

let h = g 6

let () = show_int (6 - 3)
let () = show_int (g 6 3)
let () = show_int (h 3)

let () = print_newline ()
let () = print_string "currified: "

let f1 = fun x -> fun y -> x * y
let f2 = f1 6

let () = show_int (f2 7)

let () = print_newline ()
let () = print_string "higher-order: "

let double f x = f (f x)
let add2n n x = double (( + ) n) x

let () = show_int (add2n 20 2)
let () = show_int (double double double double (( + ) 1) 0)
let () = show_int (if false then 17 else 42)
let () = show_int (if true then 17 else 42)

let f x = let x = x + x in x + x + x
let () = show_int (f 7)

let () = print_newline ()
let () = print_string "local: "

let () =
  let twice x = x + x in show_int (twice 21)

let () = print_newline ()
let () = print_string "recursive: "

let () =
  let n = 10 in
  let rec sum i = if i = n then 0 else i + sum (i + 1) in
  show_int (sum 0)

let () =
  let n = 10 in
  let rec sum1 i = if i = n then 0 else i + sum2 (i + 1)
  and sum2 i = if i = n then 0 else sum1 (i + 1) in
  show_int (sum1 0); show_int (sum2 0)

(* regression test for nested let-rec *)
let () =
  let f () =
    let rec g i = i in
    g 0
  in
  show_int (f ())

let () = print_newline ()
let () = print_string "let-binding tests: "

let () = show_int (let a = 17 in let b = 42 in if (let x = 2 in true) then a else b)
let () = show_int (let a = 17 in let b = 42 in if (let x = 2 in false) then a else b)

(* this ensures that the 'let' are not substituted away... as long as we don't support constant-folding *)
let () = show_int (let a = 16+1 in let b = 41+1 in if (let x = 1+1 in true) then a else b)
let () = show_int (let a = 16+1 in let b = 41+1 in if (let x = 1+1 in false) then a else b)

(* regression test for an infinite loop in 'Subst unfolding *)
let () = show_int (let x = 21 in let y = x in let x = y in x + y)

let () = print_newline ()
let () = print_string "more recursion: "

let rec go n =
  if n = 0 then () else (show_int n; go (n - 1))

let () = go 10

let () = print_newline ()
let () = print_string "general function application: "

let () =
  let f = ref ( + ) in
  show_int (!f 12 30)

let () = print_newline ()
