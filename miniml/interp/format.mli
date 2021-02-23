type formatter = { out_string : string -> unit; }
val mkprintf :
  bool -> ('a -> string -> unit) -> 'a -> string -> (unit -> 'b) -> 'c

val std_formatter : formatter
val err_formatter : formatter

val fprintf : formatter -> string -> 'a
val printf : string -> 'a
val eprintf : string -> 'a
val kbprintf : (Buffer.t -> 'a) -> Buffer.t -> string -> 'b
val bprintf : Buffer.t -> string -> 'a
val kprintf : (string -> 'a) -> string -> 'b
val ksprintf : (string -> 'a) -> string -> 'b
val kasprintf : (string -> 'a) -> string -> 'b
val sprintf : string -> 'a

val pp_print_cut : formatter -> unit -> 'a
val pp_print_list :
  ?pp_sep:(formatter -> unit -> 'a) ->
  (formatter -> 'b -> unit) -> formatter -> 'b list -> unit
