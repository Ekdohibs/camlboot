(* type out_channel *)
external caml_ml_open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"
external caml_ml_output : out_channel -> string -> int -> int -> unit = "caml_ml_output"
external caml_ml_flush : out_channel -> unit = "caml_ml_flush"
external caml_ml_bytes_length : string -> int = "caml_ml_bytes_length"
external format_int : string -> int -> string = "caml_format_int"
external ( ~- ) : int -> int = "%109"
external ( + ) : int -> int -> int = "%110"
external ( - ) : int -> int -> int = "%111"
external ( * ) : int -> int -> int = "%112"
external ( / ) : int -> int -> int = "%113"
external ( mod ) : int -> int -> int = "%114"
external ( land ) : int -> int -> int = "%115"
external ( lor ) : int -> int -> int = "%116"
external ( lxor ) : int -> int -> int = "%117"
external ( lsl ) : int -> int -> int = "%118"
external ( lsr ) : int -> int -> int = "%119"
external ( asr ) : int -> int -> int = "%120"
external ( = ) : 'a -> 'a -> bool = "caml_equal"
external ( <> ) : 'a -> 'a -> bool = "caml_notequal"
external ( > ) : 'a -> 'a -> bool = "caml_greaterthan"
external ( >= ) : 'a -> 'a -> bool = "caml_greaterequal"
external ( < ) : 'a -> 'a -> bool = "caml_lessthan"
external ( <= ) : 'a -> 'a -> bool = "caml_lessequal"
external raise : exn -> 'a = "%91"

let failwith s = raise (Failure s)
let invalid_arg s = raise (Invalid_argument s)
let assert b = if b then () else raise (Assert_failure ("", 0, 0))
let fst (a, b) = a
let snd (a, b) = b

let stdout = caml_ml_open_descriptor_out 1

let flush () = caml_ml_flush stdout

let print s = caml_ml_output stdout s 0 (caml_ml_bytes_length s)

let print_int n = print (format_int " %d" n)

let print_newline () =
  print "\n";
  flush ()

let print_endline s =
  print s ;
  print "\n" ;
  flush ()

(* various types used in the tests *)

(* variants *)
type bool = false | true
type 'a list = [] | (::) of 'a * 'a list
type 'a option = None | Some of 'a

(* synonyms *)
type 'a t = 'a * int
