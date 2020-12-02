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
| Empty
| Leaf of 'a
| Node of 'a tree * 'a tree

let () =
  print_int (match Node (Leaf 1, Leaf 2) with
    | Empty -> 4
    | Leaf _ -> 4
    | Node _ -> 5 (* note: a single wildcard for several arguments *)
  )

let test_nested_patterns =
  match Node(Leaf 0, Node(Leaf 8, Node(Leaf 2, Empty))) with
  | Empty -> 0
  | Leaf _ -> 0
  | Node(_, Empty) -> 0
  | Node(Empty, (Node _)) -> 1
  | Node((Node _), (Node _)) -> 1
  | Node((Leaf(0)), (Node (_, (Node (_, (Node _)))))) -> 2
  | Node((Leaf(0)), (Node ((Leaf x), (Node ((Leaf y), Empty))))) -> x - y
  | Node((Leaf(0)), (Node (_, Empty))) -> 4
  | Node (_, _) -> 5

let () = print_int test_nested_patterns

let () = print_newline ()
