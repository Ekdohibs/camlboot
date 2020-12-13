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

module N = struct
  let f ~x ?(y=2) p = p (x * y)
end

let () =
  let open N in
  f ~x:21 print_int

let () = print_newline ()
