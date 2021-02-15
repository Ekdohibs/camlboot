let () = print_endline "Arithmetic:"

let () = show_int (6 * 7)
let () = show_int (17 + 12)
let () = show_int (7 - 5)
let () = show_int (19 / 3)
let () = show_int (-2 + 3)
let () = show_int (19 mod 3)
let () = show_int (3 land 5)
let () = show_int (3 lor 5)
let () = show_int (3 lxor 5)
let () = show_int (7 lsl 1)
let () = show_int (7 lsr 1)
let () = show_int (7 asr 1)
let () = show_int (-1 lsr 1)
let () = show_int (1 lsl 62 - 1) (* Should be previous number *)
let () = show_int (-1 asr 1)

let () = print_newline ()

let () = show_int (4 - 2 - 1)
let () = show_int (0 * 3 + 2)
let () = show_int (1 + 1 * 2)

let () = print_newline ()

let ( % ) a b = a mod b

let () = show_int (3 % 2)
let () = show_int (1 + 3 % 2)
let () = show_int (7 % 4 % 4)

let () = print_newline ()

let () = show_int 0xff
let () = show_int 0xFF
let () = show_int 255
let () = show_int 0o377
let () = show_int 0b11111111

let () = show_int 0x2a
let () = show_int 42
let () = show_int 0o52
let () = show_int 0b101010

let () = print_newline ()

external int64_format : string -> int64 -> string = "caml_int64_format"
external int32_format : string -> int32 -> string = "caml_int32_format"
external nativeint_format : string -> nativeint -> string = "caml_nativeint_format"

let () = print_endline (int64_format "%d" 9223372036854775807L)
let () = print_endline (int32_format "%d" 2147483647l)
let () = print_endline (nativeint_format "%d" 9223372036854775807n)
