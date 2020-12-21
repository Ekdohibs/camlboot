let () = print_endline "Lists:"

let rec iter f l =
  match l with
  | [] -> ()
  | x :: l ->
    f x; iter f l

let print_list l =
  print_string "["; iter show_int l; print_string "]"

let () = print_list [1; 2; 3; 4; 5; 6; 7; 8; 9]

let () = print_newline ()

let rec iter_sep f sep l =
  match l with
  | [] -> ()
  | [x] ->
    f x
  (* this function is an excuse to test literral patterns
     [p1; p2; ...; pn] *)
  | [x0; x1] ->
    f x0; sep (); f x1
  | x :: l ->
    f x; sep (); iter_sep f sep l

let print_list l =
  print_string "["; iter_sep show_int (fun () -> print_string ";") l; print_string "]"

let () = print_list [1; 2; 3; 4; 5; 6; 7; 8; 9]

let () = print_newline ()

let rec map f l =
  match l with
  | [] -> []
  | x :: l -> f x :: map f l

let () = print_list (map (fun x -> x + 1) [1; 2; 3; 4; 5; 6; 7; 8; 9])

let () = print_list (map (fun (x, y) -> x + y) [(1, 1); (2, 2); (3, 3)])

let () = print_newline ()
