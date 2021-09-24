val trace : bool
val traceend : bool
val tracearg_from : int64
val tracecur : int64 ref
val tracedepth : int ref
val debug : bool
val stdlib_path : unit -> string
val compiler_source_path : unit -> string
type command = Ocamlc | Ocamlopt | Files
val command : unit -> command option
