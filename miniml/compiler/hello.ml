(* type out_channel *)
external caml_ml_open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"
external caml_ml_output : out_channel -> string -> int -> int -> unit = "caml_ml_output"
external caml_ml_flush : out_channel -> unit = "caml_ml_flush"
external caml_ml_bytes_length : string -> int = "caml_ml_bytes_length"
external format_int : string -> int -> string = "caml_format_int"
external plus : int -> int -> int = "%110"
external minus : int -> int -> int = "%111"
external times : int -> int -> int = "%112"
external eq : 'a -> 'a -> bool = "caml_equal"
external raise : exn -> 'a = "%91"
type bool = false | true
type t = { a : int ; b : int }
type 'a list = Null | Cons of 'a * 'a list
type 'a option = None | Some of 'a

let stdout = caml_ml_open_descriptor_out 1

let print s = caml_ml_output stdout s 0 (caml_ml_bytes_length s)

let print_int n = print (format_int " %d" n)

let _ = print "Hello, world!\n"

let _ = print_int (6 * 7)
let _ = print_int (17 + 12)

(* let g x = let z = x * 2 in fun y -> z * 3 *)

let g x y = x - y

let h = g 6

let _ = print_int (6 - 3)
let _ = print_int (g 6 3)
let _ = print_int (h 3)

let f1 = fun x -> fun y -> x * y
let f2 = f1 6

let _ = print_int (f2 7)

let double f x = f (f x)
let add2n n x = double (plus n) x

let _ = print_int (add2n 20 2)
let _ = print_int (double double double double (plus 1) 0)
let _ = print_int (if false then 17 else 42)
let _ = print_int (if true then 17 else 42)

let f x = let x = x + x in x + x + x
let _ = print_int (f 7)

let _ =
  let twice x = x + x in print_int (twice 21)

let _ =
  let u = { a = 5 ; b = 7 } in
  print_int u.a; print_int u.b

let _ =
  let u = { b = 5 ; a = 7 } in
  print_int u.a; print_int u.b


let _ = print_int (let a = 17 in let b = 42 in if (let x = 2 in true) then a else b)
let _ = print_int (let a = 17 in let b = 42 in if (let x = 2 in false) then a else b)

let _ = caml_ml_flush stdout

let rec go n =
  if n = 0 then () else (print_int n; go (n - 1))

let _ = go 10
let _ = caml_ml_flush stdout

let _ =
  print_int (match Null with Null -> 2 | Cons (x, l) -> 3)

let _ =
  print_int (match Cons (1, Null) with Null -> 2 | Cons (x, l) -> 3)

let rec iter f l =
  match l with
  | [] -> ()
  | x :: l ->
    f x; iter f l

let print_list l =
  print "["; iter print_int l; print "]"

let _ = print_list [1; 2; 3; 4; 5; 6; 7; 8; 9]

let rec map f l =
  match l with
  | [] -> []
  | x :: l -> f x :: map f l

let _ = print_list (map (fun x -> x + 1) [1; 2; 3; 4; 5; 6; 7; 8; 9])


let f1 ~x ~y = print_int (x + 2 * y)
let _ = f1 0 1
let _ = f1 ~x:0 ~y:1
let _ = f1 ~y:1 ~x:0

let f2 ?(x=1) ~y = print_int (x + 2 * y)
let _ = f2 100
let _ = f2 ~y:101 (* Note: this is different from ocaml *)
let _ = f2 ?x:None ~y:102
let _ = f2 ?x:(Some 0) ~y:103
let _ = f2 ~x:0 ~y:104

let _ =
  let u = { a = 5 ; b = 7 } in
  let v = { u with a = 42 } in
  let w = { u with b = 16 } in
  print_int u.a; print_int u.b;
  print_int v.a; print_int v.b;
  print_int w.a; print_int w.b

exception E1
exception E2 of int
exception E3
exception E4 of int

let show_exn e =
  match e with
  | E1 -> print "E1"
  | E2 i -> print "(E2"; print_int i; print ")"
  | _ -> print "<unknown>"

let _ =
  try raise E1 with
  | E1 -> print " ok"
  | _ -> print " ko"

let _ =
  try raise (E2 7) with
  | E2 x -> if x = 7 then print " ok" else print " ko"
  | _ -> print " ko"

let _ = print (try " ok" with _ -> " ko")

let _ = try (try raise E1 with E2 _ -> print " ko") with E1 -> print " ok" | _ -> print " ko"
let _ = try (try raise (E2 7) with E1 -> print " ko") with E2 x -> if x = 7 then print " ok" else print " ko" | _ -> print " ko"
let _ = try (try raise E3 with E1 -> print " ko" | E2 _ -> print " ko") with _ -> print " ok"
let _ = try (try raise (E4 7) with E1 -> print " ko" | E2 _ -> print " ko") with _ -> print " ok"

let _ = try (try raise E1 with E1 -> print " ok") with _ -> print " ko"
let _ = try (try raise (E2 7) with E2 x -> if x = 7 then print " ok" else print " ko") with _ -> print " ko"

let _ = print " "; show_exn E1
let _ = print " "; show_exn (E2 7)
let _ = print " "; show_exn E3
let _ = print " "; show_exn (E4 7)

module M = struct
  let x = 42
  let f x = x + x
end

let _ =
  print_int M.x;
  let open M in
  print_int (f 42)

let _ = caml_ml_flush stdout
