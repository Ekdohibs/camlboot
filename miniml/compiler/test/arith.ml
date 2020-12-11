let () = print_endline "Arithmetic:"

let () = print_int (6 * 7)
let () = print_int (17 + 12)
let () = print_int (7 - 5)
let () = print_int (19 / 3)
let () = print_int (- (2) + 3) (* parentheses so that it is not parsed as a negative number *)
let () = print_int (19 mod 3)
let () = print_int (3 land 5)
let () = print_int (3 lor 5)
let () = print_int (3 lxor 5)
let () = print_int (7 lsl 1)
let () = print_int (7 lsr 1)
let () = print_int (7 asr 1)
let () = print_int (-1 lsr 1)
let () = print_int (1 lsl 62 - 1) (* Should be previous number *)
let () = print_int (-1 asr 1)

let () = print_newline ()
