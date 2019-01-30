exception Not_found
exception End_of_file
exception Failure of string
let false = 0
let true = 1

module Obj = struct let repr x = x let magic x = x end
let lazy x = x
module Lazy = struct let force x = x end

module List = struct
  let hd x = match x with [] -> assert false | a :: b -> a
  let rec filter f l = match l with [] -> [] | x :: l -> if f x then x :: filter f l else filter f l
  let rec map f l = match l with [] -> [] | x :: l -> f x :: map f l
  let rec fold_left f acc l = match l with [] -> acc | x :: l -> fold_left f (f acc x) l
  let rec fold_right f l acc = match l with [] -> acc | x :: l -> f x (fold_right f l acc)
end

let rec list_concat l1 l2 = match l1 with [] -> l2 | x :: l1 -> x :: list_concat l1 l2

external compare : = "caml_compare"
external eq : = "caml_equal"
external lessequal : = "caml_lessequal"
