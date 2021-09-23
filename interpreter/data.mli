module SMap : Map.S with type key = string
module SSet : Set.S with type elt = string
type module_unit_id = Path of string
module UStore : Map.S with type key = module_unit_id

module Ptr :
  sig
    type 'a t
    val create : 'a -> 'a t
    exception Null
    val get : 'a t -> 'a
    val dummy : unit -> 'a t
    exception Full
    val backpatch : 'a t -> 'a -> unit
  end

val ptr : 'a -> 'a Ptr.t
val onptr : ('a -> 'b) -> 'a Ptr.t -> 'b

type value = value_ Ptr.t
and value_ =
    Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Fun of Asttypes.arg_label * Parsetree.expression option *
      Parsetree.pattern * Parsetree.expression * env
  | Function of Parsetree.case list * env
  | String of bytes
  | Float of float
  | Tuple of value list
  | Constructor of string * int * value option
  | Prim of (value -> value)
  | Fexpr of fexpr
  | ModVal of mdl
  | InChannel of in_channel
  | OutChannel of out_channel
  | Record of value ref SMap.t
  | Lz of (unit -> value) ref
  | Array of value array
  | Fun_with_extra_args of value * value list *
      (Asttypes.arg_label * value) SMap.t
  | Object of object_value

and fexpr =
    Location.t ->
    (Asttypes.arg_label * Parsetree.expression) list ->
    Parsetree.expression option

and 'a env_map = (bool * 'a) SMap.t

and env = {
  values : value_or_lvar env_map;
  modules : mdl env_map;
  constructors : int env_map;
  classes : class_def env_map;
  current_object : object_value option;
}

and value_or_lvar =
    Value of value
  | Instance_variable of object_value * string

and class_def = Parsetree.class_expr * env ref

and mdl =
    Unit of module_unit_id * module_unit_state ref
  | Module of mdl_val
  | Functor of string * Parsetree.module_expr * env

and mdl_val = {
  mod_values : value SMap.t;
  mod_modules : mdl SMap.t;
  mod_constructors : int SMap.t;
  mod_classes : class_def SMap.t;
}

and module_unit_state = Not_initialized_yet | Initialized of mdl_val

and object_value = {
  env : env;
  self : Parsetree.pattern;
  initializers : expr_in_object list;
  named_parents : object_value SMap.t;
  variables : value ref SMap.t;
  methods : expr_in_object SMap.t;
  parent_view : string list;
}

and source_object = Current_object | Parent of object_value

and expr_in_object = {
  source : source_object;
  instance_variable_scope : SSet.t;
  named_parents_scope : SSet.t;
  expr : Parsetree.expression;
}

exception InternalException of value

val unit : value_ Ptr.t
val is_true : value_ Ptr.t -> bool
val pp_print_value : Format.formatter -> value_ Ptr.t -> unit
val pp_print_unit_id : Format.formatter -> module_unit_id -> unit
val read_caml_int : string -> int64
val value_of_constant : Parsetree.constant -> value_ Ptr.t
val value_compare : value_ Ptr.t -> value_ Ptr.t -> int
val value_equal : value_ Ptr.t -> value_ Ptr.t -> bool
val value_lt : value_ Ptr.t -> value_ Ptr.t -> bool
val value_le : value_ Ptr.t -> value_ Ptr.t -> bool
val value_gt : value_ Ptr.t -> value_ Ptr.t -> bool
val value_ge : value_ Ptr.t -> value_ Ptr.t -> bool
val next_exn_id : unit -> int
exception No_module_data
val get_module_data : Location.t -> mdl -> mdl_val
val module_name_of_unit_path : string -> string
