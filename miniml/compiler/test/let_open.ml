let () = print_endline "Let open:"

module M = struct
  let x = 42
  let f x = x + x
end

let () =
  show_int M.x;
  M.(show_int x);
  let open M in
  show_int (f 21)

module N = struct
  let f ~x ?(y=2) p = p (x * y)
end

let () =
  let open N in
  f ~x:21 show_int

let () = print_newline ()
