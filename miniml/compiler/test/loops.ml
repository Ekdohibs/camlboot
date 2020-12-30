let () =
  print_endline "nonempty while";
  let x = ref 42 in
  while !x > 0 do
    show_int !x;
    x := !x / 2
  done;
  print_newline ()

let () =
  print_endline "empty while";
  while false do
    show_int 42
  done;
  print_newline ()

let () =
  print_endline "nonempty for (up)";
  for x = 0 to 10 do
    show_int x
  done;
  print_newline ()

let () =
  print_endline "nonempty for (down)";
  for x = 10 downto 0 do
    show_int x
  done;
  print_newline ()

let () =
  print_endline "one-iteration for";
  for x = 42 to 42 do
    show_int x
  done;
  for x = 42 downto 42 do
    show_int x
  done;
  print_newline ()

let () =
  print_endline "empty for";
  for x = 1 to 0 do
    show_int x
  done;
  for x = 0 downto 1 do
    show_int x
  done;
  print_newline ()
