let _ = print_endline "Functions:"

let () = print "simple: "
(* let g x = let z = x * 2 in fun y -> z * 3 *)

let g x y = x - y

let h = g 6

let () = print_int (6 - 3)
let () = print_int (g 6 3)
let () = print_int (h 3)

let () = print_newline ()
let () = print "currified: "

let f1 = fun x -> fun y -> x * y
let f2 = f1 6

let () = print_int (f2 7)

let () = print_newline ()
let () = print "higher-order: "

let double f x = f (f x)
let add2n n x = double (( + ) n) x

let () = print_int (add2n 20 2)
let () = print_int (double double double double (( + ) 1) 0)
let () = print_int (if false then 17 else 42)
let () = print_int (if true then 17 else 42)

let f x = let x = x + x in x + x + x
let () = print_int (f 7)

let () = print_newline ()
let () = print "local: "

let () =
  let twice x = x + x in print_int (twice 21)

let () = print_newline ()
let () = print "recursive: "

let () =
  let n = 10 in
  let rec sum i = if i = n then 0 else i + sum (i + 1) in
  print_int (sum 0)

let () =
  let n = 10 in
  let rec sum1 i = if i = n then 0 else i + sum2 (i + 1)
  and sum2 i = if i = n then 0 else sum1 (i + 1) in
  print_int (sum1 0); print_int (sum2 0)

let () = print_newline ()
let () = print "let-binding tests: "

let () = print_int (let a = 17 in let b = 42 in if (let x = 2 in true) then a else b)
let () = print_int (let a = 17 in let b = 42 in if (let x = 2 in false) then a else b)

(* this ensures that the 'let' are not substituted away... as long as we don't support constant-folding *)
let () = print_int (let a = 16+1 in let b = 41+1 in if (let x = 1+1 in true) then a else b)
let () = print_int (let a = 16+1 in let b = 41+1 in if (let x = 1+1 in false) then a else b)

(* regression test for an infinite loop in 'Subst unfolding *)
let () = print_int (let x = 21 in let y = x in let x = y in x + y)

let () = print_newline ()
let () = print "more recursion: "

let rec go n =
  if n = 0 then () else (print_int n; go (n - 1))

let () = go 10

let () = print_newline ()
