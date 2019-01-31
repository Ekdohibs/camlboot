exception Not_found
exception End_of_file
exception Failure of string
exception Invalid_argument of string
let invalid_arg x = raise (Invalid_argument x)
let failwith x = raise (Failure x)

type bool = false | true
type ref = { mutable contents : 'a }

module Obj = struct
  let repr x = x
  let magic x = x
  external is_block : = "caml_obj_is_block"
  external tag : = "caml_obj_tag"
end
let lazy x = x
module Lazy = struct let force x = x end

module List = struct
  let hd x = match x with [] -> assert false | a :: b -> a
  let rec filter f l = match l with [] -> [] | x :: l -> if f x then x :: filter f l else filter f l
  let rec map f l = match l with [] -> [] | x :: l -> f x :: map f l
  let rec fold_left f acc l = match l with [] -> acc | x :: l -> fold_left f (f acc x) l
  let rec fold_right f l acc = match l with [] -> acc | x :: l -> f x (fold_right f l acc)
end

module Bytes = struct
  external blit : = "caml_blit_bytes"
  external unsafe_blit : = "caml_blit_bytes"
  external blit_string : = "caml_blit_string"
  external create : = "caml_create_bytes"
  external get : = "caml_bytes_get"
  external unsafe_get : = "caml_bytes_get"
  external unsafe_set : = "caml_bytes_set"
  external unsafe_of_string : = "caml_bytes_of_string"
  external unsafe_to_string : = "caml_string_of_bytes"
  external length : = "caml_ml_bytes_length"

  let copy s =
    let len = length s in
    let r = create len in
    unsafe_blit s 0 r 0 len;
    r

  let to_string b = unsafe_to_string (copy b)
  let of_string s = copy (unsafe_of_string s)

  let sub s ofs len =
    if ofs < 0 || len < 0 || ofs > length s - len
    then invalid_arg "String.sub / Bytes.sub"
    else begin
      let r = create len in
      unsafe_blit s ofs r 0 len;
      r
    end

  let sub_string b ofs len = unsafe_to_string (sub b ofs len)
end

module Char = struct let code x = x let chr x = x let unsafe_chr x = x end
module Uchar = struct let unsafe_of_int x = x let to_int x = x let is_valid x = true end

let rec list_concat l1 l2 = match l1 with [] -> l2 | x :: l1 -> x :: list_concat l1 l2

external compare : = "caml_compare"
external eq : = "caml_equal"
external neq : = "caml_notequal"
external lessequal : = "caml_lessequal"
external lessthan : = "caml_lessthan"

let ref x = { contents = x }
let ref_get x = x.contents
let ref_set x y = x.contents <- y
let incr x = ref_set x (ref_get x + 1)
let not x = 1 - x

module Array = struct
  external blit : = "caml_array_blit"
  external make : = "caml_make_vect"
end

external array_get : = "caml_array_get"
external array_set : = "caml_array_set"
external int_of_string : = "caml_int_of_string"
