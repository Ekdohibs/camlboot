let () = print_endline "Pattern-matching:"

let () =
  print_int (match [] with [] -> 2 | x :: l -> 3)

let () =
  print_int (match 1 :: [] with
    | [] -> 2 (* note: leading bar *)
    | _ :: _ -> 3
  )

let test_function = function
  | [] -> 2
  | x :: _ -> x + 1 (* note: one of the pattern arguments is a wildcard *)

let () =
  print_int (test_function (3 :: []))

type 'a tree =
| Leaf of 'a
| Node of 'a tree * 'a tree

let () =
  print_int (match Node (Leaf 1, Leaf 2) with
    | Leaf _ -> 4
    | Node _ -> 5 (* note: a single wildcard for several arguments *)
  )

let () = print_newline ()
