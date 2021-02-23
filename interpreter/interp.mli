open Data

val parse : string -> Parsetree.structure
type env_flag = Open of Longident.t
val stdlib_flag : env_flag list
val no_stdlib_flag : 'a list
val stdlib_units : (env_flag list * string) list
val eval_env_flag : loc:Location.t -> env -> env_flag -> env
val load_rec_units :
  env -> (env_flag list * string) list -> env
val stdlib_env : env
module Compiler_files :
  sig
    val utils : string list
    val parsing : string list
    val pure_typing : string list
    val lambda : string list
    val more_typing : string list
    val bytecomp : string list
    val driver : string list
    val middle_end : string list
    val asmcomp : string list
    val bytegen : string list
    val bytecode_main : string list
    val native_main : string list
  end
val bytecode_compiler_units : (env_flag list * string) list
val native_compiler_units : (env_flag list * string) list
val run_ocamlc : unit -> unit
val run_ocamlopt : unit -> unit
val run_files : unit -> unit
