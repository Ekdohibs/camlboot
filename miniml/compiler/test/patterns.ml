let () = print_endline "Pattern-matching:"

let () = print_string "simple: "

let () =
  show_int (match [] with [] -> 2 | x :: l -> 3)

let () =
  show_int (match 1 :: [] with
    | [] -> 2 (* note: leading bar *)
    | _ :: _ -> 3
  )

let test_function = function
  | [] -> 2
  | x :: _ -> x + 1 (* note: one of the pattern arguments is a wildcard *)

let () =
  show_int (test_function (3 :: []))

type 'a tree =
| Empty
| Leaf of 'a
| Node of 'a tree * 'a tree

let () =
  show_int (match Node (Leaf 1, Leaf 2) with
    | Empty -> 4
    | Leaf _ -> 4
    | Node _ -> 5 (* note: a single wildcard for several arguments *)
  )

let () = print_newline ()
let () = print_string "irrefutable patterns in let-bindings: "

let () = show_int (
  let (a, b) = (2, 3) in b - a
)

let () = print_newline ()
let () = print_string "nested patterns: "

let test_nested_patterns =
  match Node(Leaf 0, Node(Leaf 8, Node(Leaf 2, Empty))) with
  | Empty -> 0
  | Leaf _ -> 0
  | Node(_, Empty) -> 0
  | Node(Empty, Node _) -> 1
  | Node(Node _, Node _) -> 1
  | Node(Leaf 0, Node (_, Node (_, Node _))) -> 2
  | Node(Leaf 0, Node (Leaf x, Node (Leaf y, Empty))) -> x - y
  | Node(Leaf 0, Node (_, Empty)) -> 4
  | Node (a, b) ->
    (match Node (a, b) with
     | Node (Empty, _) -> 0
     | Node (_, Empty) -> 0
     | Node (Leaf _, Leaf _) -> 1
     | _ -> 2)

let () = show_int test_nested_patterns

let () = print_newline ()
let () = print_string "as-patterns: "

let () = show_int (match (2, 3) with
  | (_ as a, _) as p ->
    let (_, b) = p in
    b - a
)

let () = print_newline ()
let () = print_string "or-patterns: "

(* toplevel ors, no parentheses *)
let () = show_int (match 1 with
  | 0 | 1 | 2 -> 1 (* no parentheses *)
  | 3 | 4 -> 2
  | 5 | _ -> 3
)

(* toplevel ors, with parentheses *)
let () = show_int (match 3 with
  | (0 | 1 | 2) -> 1
  | (3 | 4) -> 2 (* parentheses *)
  | 5 | _ -> 3
)

(* in-depth ors *)
let () = show_int (match (2, 3) with
  | ((0 | 1), _) -> 1
  | (2, (0 | 1)) -> 2
  | (2, (2 | 3)) -> 3
  | ((3 | 4), _) -> 4
  | _ -> 5
)

(* oring constant and non-constant patterns *)
let () = show_int (match Node (Empty, Empty) with
  | Empty | Leaf _ -> 0
  | Node ((Empty | Leaf _), Node _) -> 1
  | Node (_, (Empty | Leaf _)) -> 4
  | Node (Node _, Node _) -> 12
)

let () = print_newline ()
let () = print_string "record patterns"

type ('a, 'b) t = { a : 'a; b : 'b }
let () = show_int (match { a = Empty; b = Leaf 1 } with
  | { a = (Leaf _ | Node _) } -> 0
  | { b = (Empty | Node _) } -> 2
  | { a = Empty; b = Leaf n } -> n
)

let () = print_newline ()
let () = print_string "string patterns"
let f = function
| "foo" -> 42
| "barbar" -> 21
| _ -> 0

let () = show_int (f "foo")
let () = show_int (2 * f "barbar")
let () = show_int (42 + f "bar")

let () = print_newline ()
let () = print_string "when-guards"

let () = show_int (match Node (Leaf 2, Leaf 2) with
  | Node _ when (show_int 1; false) -> 0
  | Node (Leaf n, (Leaf _ | Empty)) when (show_int n; false) -> 0
  | Node _ -> 3
  | _ -> 4
)

let () = print_newline ()
let () = print_string "match with exception"

let f ~success:b =
  if b then 42
  else raise (Failure " 42")

let () = match f ~success:true with
  | n -> show_int n
  | exception (Failure s) -> print_string s

let () = match f ~success:false with
  | n -> show_int n
  | exception (Failure s) -> print_string s

let () = print_newline ()
