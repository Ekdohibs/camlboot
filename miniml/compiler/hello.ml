(* type out_channel *)
external caml_ml_open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"
external caml_ml_output : out_channel -> string -> int -> int -> unit = "caml_ml_output"
external caml_ml_flush : out_channel -> unit = "caml_ml_flush"
external caml_ml_bytes_length : string -> int = "caml_ml_bytes_length"
external format_int : string -> int -> string = "caml_format_int"
external ( + ) : int -> int -> int = "%110"
external ( - ) : int -> int -> int = "%111"
external ( * ) : int -> int -> int = "%112"
external div : int -> int -> int = "%113"
external ( = ) : 'a -> 'a -> bool = "caml_equal"
external raise : exn -> 'a = "%91"

let stdout = caml_ml_open_descriptor_out 1

let print s = caml_ml_output stdout s 0 (caml_ml_bytes_length s)

let print_int n = print (format_int " %d" n)

let _ = print "\nType declarations\n"

(* variants *)
type bool = false | true
type 'a list = [] | (::) of 'a * 'a list
type 'a option = None | Some of 'a

(* records *)
type t = { a : int ; b : int }

(* synonyms *)
type 'a t = 'a * int


let _ = print "Hello, world!\n"

let _ = print_int (6 * 7)
let () = print_int (17 + 12)

(* let g x = let z = x * 2 in fun y -> z * 3 *)

let g x y = x - y

let h = g 6

let () = print_int (6 - 3)
let () = print_int (g 6 3)
let () = print_int (h 3)

let f1 = fun x -> fun y -> x * y
let f2 = f1 6

let () = print_int (f2 7)

let double f x = f (f x)
let add2n n x = double (( + ) n) x

let () = print_int (add2n 20 2)
let () = print_int (double double double double (( + ) 1) 0)
let () = print_int (if false then 17 else 42)
let () = print_int (if true then 17 else 42)

let f x = let x = x + x in x + x + x
let () = print_int (f 7)

let () =
  let twice x = x + x in print_int (twice 21)

let () =
  let n = 10 in
  let rec sum i = if i = n then 0 else i + sum (i + 1) in
  print_int (sum 0)

let () =
  let n = 10 in
  let rec sum1 i = if i = n then 0 else i + sum2 (i + 1)
  and sum2 i = if i = n then 0 else sum1 (i + 1) in
  print_int (sum1 0); print_int (sum2 0)

let () = print_int (let a = 17 in let b = 42 in if (let x = 2 in true) then a else b)
let () = print_int (let a = 17 in let b = 42 in if (let x = 2 in false) then a else b)

let () = caml_ml_flush stdout

let rec go n =
  if n = 0 then () else (print_int n; go (n - 1))

let () = go 10
let () = caml_ml_flush stdout

let () = print "\nPattern-matching:\n"

let () =
  print_int (match [] with [] -> 2 | x :: l -> 3)

let () =
  print_int (match 1 :: [] with
    | [] -> 2 (* note: leading bar *)
    | x :: l -> 3)

let test_function = function
  | [] -> 2
  | x :: l -> x + 1

let () =
  print_int (test_function (3 :: []))

let () = print "\nLists:\n"

let rec iter f l =
  match l with
  | [] -> ()
  | x :: l ->
    f x; iter f l

let print_list l =
  print "["; iter print_int l; print "]"

let () = print_list [1; 2; 3; 4; 5; 6; 7; 8; 9]

let rec map f l =
  match l with
  | [] -> []
  | x :: l -> f x :: map f l

let () = print_list (map (fun x -> x + 1) [1; 2; 3; 4; 5; 6; 7; 8; 9])

let () = print_list (map (fun (x, y) -> x + y) [(1, 1); (2, 2); (3, 3)])

let () = print "\nArguments:\n"

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

let () = print "\nRecords:\n"

let () =
  let u = { a = 5 ; b = 7 } in
  print_int u.a; print_int u.b

let () =
  let u = { b = 5 ; a = 7 } in
  print_int u.a; print_int u.b

let () =
  let u = { a = 5 ; b = 7 } in
  let v = { u with a = 42 } in
  let w = { u with b = 16 } in
  print_int u.a; print_int u.b;
  print_int v.a; print_int v.b;
  print_int w.a; print_int w.b

let () = print "\nExceptions:\n"

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

let () = print "\nlet open:\n"

module M = struct
  let x = 42
  let f x = x + x
end

let () =
  print_int M.x;
  M.(print_int x);
  let open M in
  print_int (f 21)

let () = print "\nInfix operators treated as sugar:\n"

let succ n = n + 1
let ignore_and_print_int () n = print_int n
let () = ignore_and_print_int () @@ succ @@ 1
let () = 2 |> succ |> ignore_and_print_int ()

let () = print "\nExternally raised exceptions:\n"

external obj_tag : Obj.t -> int = "caml_obj_tag"
external obj_size : Obj.t -> int = "%79"
external obj_field : Obj.t -> int -> Obj.t = "%80"

let rec print_obj x =
  let t = obj_tag x in
  if t = 1000 then print (format_int "%d" x)
  else if t = 1001 then print "<out of heap>"
  else if t = 1002 then print "<unaligned>"
  else if t = 252 then (print "\""; print x; print "\"")
  else (print (format_int "%d" t); print "["; print_obj_fields x 0; print "]")

and print_obj_fields x i =
  if i = obj_size x then ()
  else if i = obj_size x - 1 then print_obj (obj_field x i)
  else (print_obj (obj_field x i); print " "; print_obj_fields x (i + 1))

let print_exn e =
  match e with
  | Out_of_memory -> print "Out_of_memory"
  | Sys_error s -> print "Sys_error \""; print s; print "\""
  | Failure s -> print "Failure \""; print s; print "\""
  | Invalid_argument s -> print "Invalid_argument \""; print s; print "\""
  | End_of_file -> print "End_of_file"
  | Division_by_zero -> print "Division_by_zero"
  | Not_found -> print "Not_found"
  | Match_failure _ -> print "Match_failure _"
  | Stack_overflow -> print "Stack overflow"
  | Sys_blocked_io -> print "Sys_blocked_io"
  | Assert_failure _ -> print "Assert_failure _"
  | Undefined_recursive_module _ -> print "Undefined_recursive_module _"
  | _ -> print "<unknown>"

let run_and_print_exn f =
  try f (); print "no exception\n" with e -> (print_obj e; print " "; print_exn e; print "\n")

external int_of_string : string -> int = "caml_int_of_string"
external sys_getenv : string -> string = "caml_sys_getenv"

let () = run_and_print_exn (fun () -> (fun x -> ()) = (fun x -> ()))
let () = run_and_print_exn (fun () -> int_of_string "fqsq")
let () = run_and_print_exn (fun () -> sys_getenv "fqsq")
let rec stack_overflow () = 1 + stack_overflow ()
let () = run_and_print_exn stack_overflow
let () = run_and_print_exn (fun () -> div 1 0)

let () = print "\n"
let () = caml_ml_flush stdout
