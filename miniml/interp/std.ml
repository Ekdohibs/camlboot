external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( ~- ) : int -> int = "%negint"
external ( mod ) : int -> int -> int = "%modint"
external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"
external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"

external compare : 'a -> 'a -> int = "caml_compare"
external ( = ) : 'a -> 'a -> bool = "caml_equal"
external ( <> ) : 'a -> 'a -> bool = "caml_notequal"
external ( <= ) : 'a -> 'a -> bool = "caml_lessequal"
external ( < ) : 'a -> 'a -> bool = "caml_lessthan"
external ( >= ) : 'a -> 'a -> bool = "caml_greaterequal"
external ( > ) : 'a -> 'a -> bool = "caml_greaterthan"

external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"


external raise : exn -> 'a = "%raise"

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'a = "%field1"

let invalid_arg x = raise (Invalid_argument x)
let failwith x = raise (Failure x)

let ignore _ = ()
let succ x = x + 1
let pred x = x - 1

let max_int = (-1) lsr 1
let min_int = max_int + 1

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

let abs x = if x < 0 then -x else x

type bool = false | true
type 'a ref = { mutable contents : 'a }
type ('a, 'b) result = Ok of 'a | Error of 'b
type 'a list = [] | (::) of 'a * 'a list
type 'a option = None | Some of 'a

let assert b = if b = 0 then raise (Assert_failure ("", 0, 0))

exception Exit

external __array_get : 'a array -> int -> 'a = "caml_array_get"
external __array_set : 'a array -> int -> 'a -> unit = "caml_array_set"
external __string_get : string -> int -> char = "caml_string_get"
external __string_set : string -> int -> char -> unit = "caml_bytes_set"

module Obj = struct
  type t
  let obj x = x
  let repr x = x
  let magic x = x
  external is_block : t -> bool = "caml_obj_is_block"
  external new_block : int -> int -> t = "caml_obj_block"
  external tag : t -> int = "caml_obj_tag"
  external set_tag : t -> int -> unit = "caml_obj_set_tag"
  external size : t -> int = "%79" (* VECTLENGTH "caml_obj_size" *)
  external field : t -> int -> t = "%80" (* GETVECTITEM "caml_obj_field" *)
  external set_field : t -> int -> t -> unit = "%81" (* SETVECTITEM "caml_obj_set_field" *)
  external is_int : t -> bool = "%129"
  let string_tag = 252
end
let lazy x = x
module Lazy = struct let force x = x end

let int_of_char x = x
let char_of_int x = x

external string_length : string -> int = "%string_length"
external bytes_length : bytes -> int = "%bytes_length"
external bytes_create : int -> bytes = "caml_create_bytes"
external string_blit : string -> int -> bytes -> int -> int -> unit = "caml_blit_string"
external bytes_blit : bytes -> int -> bytes -> int -> int -> unit = "caml_blit_bytes"
external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = bytes_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  bytes_unsafe_to_string s

(* module Char = struct let code x = x let chr x = x let unsafe_chr x = x end *)
module Uchar = struct let unsafe_of_int x = x let to_int x = x let is_valid x = true end

let rec ( @ ) l1 l2 = match l1 with [] -> l2 | x :: l1 -> x :: (l1 @ l2)

let ref x = { contents = x }
let ( ! ) x = x.contents
let ( := ) x y = x.contents <- y
let incr x = x := !x + 1
let decr x = x := !x - 1
let not x = 1 - x

external __mkatom0 : unit -> 'a array = "%58"
let __atom0 = __mkatom0 ()

external int_of_string : string -> int = "caml_int_of_string"

external unsafe_input : in_channel -> bytes -> int -> int -> unit = "caml_ml_input"

let input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "input"
  else unsafe_input ic s ofs len

external unsafe_output : out_channel -> bytes -> int -> int -> unit = "caml_ml_output_bytes"

let output oc s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "output"
  else unsafe_output oc s ofs len

external open_descriptor_out : int -> out_channel
                             = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"
type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"


external set_in_channel_name: in_channel -> string -> unit =
  "caml_ml_set_channel_name"

let open_in_gen mode perm name =
  let c = open_descriptor_in(open_desc name mode perm) in
  set_in_channel_name c name;
  c

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name
let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

external set_out_channel_name: out_channel -> string -> unit =
  "caml_ml_set_channel_name"

let open_out_gen mode perm name =
  let c = open_descriptor_out(open_desc name mode perm) in
  set_out_channel_name c name;
  c

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 438 (* 0o666 *) name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 438 (* 0o666 *) name

external close_in : in_channel -> unit = "caml_ml_close_channel"
let close_in_noerr ic = try close_in ic with _ -> ()
external flush : out_channel -> unit = "caml_ml_flush"
external close_out_channel : out_channel -> unit = "caml_ml_close_channel"
let close_out oc = flush oc; close_out_channel oc

let stdout = open_descriptor_out 1
let stderr = open_descriptor_out 2

external unsafe_output_string : out_channel -> string -> int -> int -> unit = "caml_ml_output"
external output_binary_int : out_channel -> int -> unit = "caml_ml_output_int"
external output_char : out_channel -> char -> unit = "caml_ml_output_char"
external pos_out : out_channel -> int = "caml_ml_pos_out"
external seek_out : out_channel -> int -> unit = "caml_ml_seek_out"

external input_binary_int : in_channel -> int = "caml_ml_input_int"
external input_char : in_channel -> char = "caml_ml_input_char"
external input_value : in_channel -> 'a = "caml_input_value"
external seek_in : in_channel -> int -> unit = "caml_ml_seek_in"
external pos_in : in_channel -> int = "caml_ml_pos_in"

let output_string oc s =
  unsafe_output_string oc s 0 (string_length s)

let print_string s = output_string stdout s; flush stdout
let print_newline () = print_string "\n"
let print_endline s = print_string s; print_newline ()
let print_err s = output_string stderr s; flush stderr

external unsafe_input : in_channel -> bytes -> int -> int -> int
                      = "caml_ml_input"

let input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "input"
  else unsafe_input ic s ofs len

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then () else begin
    let r = unsafe_input ic s ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic s (ofs + r) (len - r)
  end

let really_input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "really_input"
  else unsafe_really_input ic s ofs len

let really_input_string ic len =
  let s = bytes_create len in
  really_input ic s 0 len;
  bytes_unsafe_to_string s


external format_int : string -> int -> string = "caml_format_int"
let string_of_int n = format_int "%d" n

module Sys = struct
  type backend_type =
  | Native
  | Bytecode
  | Other of string

  external getenv : string -> string = "caml_sys_getenv"
  
  let backend_type = Bytecode
  let getenv_opt s =
    try Some (getenv s)
    with Not_found -> None

  let max_string_length = (1 lsl 57) - 9
  let max_array_length = (1 lsl 54) - 1
  let word_size = 8

  external get_argv: unit -> string * string array = "caml_sys_get_argv"
  let eav = get_argv ()
  let executable_name = fst eav
  let argv = snd eav
  let big_endian = false

  external file_exists: string -> bool = "caml_sys_file_exists"
  external getcwd: unit -> string = "caml_sys_getcwd"
  external rename : string -> string -> unit = "caml_sys_rename"
  external remove: string -> unit = "caml_sys_remove"

  let os_type = ""
  let ocaml_version = "camlboot"
end


external float_of_string : string -> float = "caml_float_of_string"

external sys_exit : int -> 'a = "caml_sys_exit"

external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc]
external acos : float -> float = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc]
external asin : float -> float = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc]
external atan : float -> float = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc]
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]
external hypot : float -> float -> float
               = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc]
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc]
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc]
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc]
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc]
external ceil : float -> float = "caml_ceil_float" "ceil"
  [@@unboxed] [@@noalloc]
external floor : float -> float = "caml_floor_float" "floor"
  [@@unboxed] [@@noalloc]
external abs_float : float -> float = "%absfloat"
external copysign : float -> float -> float
                  = "caml_copysign_float" "caml_copysign"
                  [@@unboxed] [@@noalloc]
external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]
external frexp : float -> float * int = "caml_frexp_float"
external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) =
  "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external float_of_bits : int64 -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

(*
let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
let nan =
  float_of_bits 0x7F_F0_00_00_00_00_00_01L
let max_float =
  float_of_bits 0x7F_EF_FF_FF_FF_FF_FF_FFL
let min_float =
  float_of_bits 0x00_10_00_00_00_00_00_00L
let epsilon_float =
  float_of_bits 0x3C_B0_00_00_00_00_00_00L
*)

external format_float : string -> float -> string = "caml_format_float"
let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | c when (c = '-' || '0' <= c && c <= '9') -> loop (i + 1)
    | _ -> s
  in
  loop 0

let string_of_float f = valid_float_lexem (format_float "%.12g" f)


module Pervasives = struct
  let compare = compare
end

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

let bool_of_string_opt = function
  | "true" -> Some true
  | "false" -> Some false
  | _ -> None

external out_channels_list : unit -> out_channel list
                           = "caml_ml_out_channels_list"

let flush_all () =
  let rec iter = function
      [] -> ()
    | a::l ->
        begin try
            flush a
        with Sys_error _ ->
          () (* ignore channels closed during a preceding flush. *)
        end;
        iter l
  in iter (out_channels_list ())

let exit_function = ref flush_all
let at_exit f =
  let g = !exit_function in
  (* MPR#7253, MPR#7796: make sure "f" is executed only once *)
  let f_already_ran = ref false in
  exit_function :=
    (fun () -> 
      if not !f_already_ran then begin f_already_ran := true; f() end;
      g())

let __atexit () = (!exit_function) ()

let exit retcode =
  __atexit ();
  sys_exit retcode
