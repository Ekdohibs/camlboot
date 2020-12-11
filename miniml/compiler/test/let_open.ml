let () = print_endline "Let open:"

module M = struct
  let x = 42
  let f x = x + x
end

let () =
  print_int M.x;
  M.(print_int x);
  let open M in
  print_int (f 21)

let () = print_newline ()
