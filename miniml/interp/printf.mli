val printf : string -> 'a
val fprintf : out_channel -> string -> 'a
val eprintf : string -> 'a
val kbprintf : (Buffer.t -> 'a) -> Buffer.t -> string -> 'b
val bprintf : Buffer.t -> string -> 'a
val kprintf : (string -> 'a) -> string -> 'b
val ksprintf : (string -> 'a) -> string -> 'b
val kasprintf : (string -> 'a) -> string -> 'b
val sprintf : string -> 'a
