let () = print_endline "Let module:"

let () =
  let module M = struct
    let x = 42
    let f x = x + x
  end in
  show_int M.x;
  M.(show_int x);
  let open M in
  show_int (f 21)

let () =
  let module Prim = struct
    external string_length : string -> int = "caml_ml_bytes_length"
    external string_create : int -> string = "caml_create_bytes"
  end in
  show_int (Prim.string_length (Prim.string_create 42))

let () = print_newline ()
