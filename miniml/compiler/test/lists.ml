let () = print_endline "Lists:"

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

let () = print_newline ()
