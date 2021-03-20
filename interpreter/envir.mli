open Data

val empty_env : env
val env_set_value : SMap.key -> value -> env -> env
val env_set_lvar : SMap.key -> object_value -> env -> env
val env_set_instance_variable :
  SMap.key -> object_value -> string -> env -> env
val env_set_module : SMap.key -> mdl -> env -> env
val env_set_constr : SMap.key -> int -> env -> env
val env_set_class : SMap.key -> class_def -> env -> env
val env_extend : bool -> env -> mdl_val -> env
val declare_unit : env -> string -> env
val define_unit : env -> string -> mdl_val -> env
val env_of_module_data : mdl_val -> env
val make_module_data : env -> mdl_val
val prevent_export : env -> env
val decompose :
  (env -> Longident.t Asttypes.loc -> mdl_val) ->
  env -> Longident.t Asttypes.loc -> string * env * string
val lookup :
  string ->
  env_name:string -> ('a * 'b) SMap.t -> SMap.key Asttypes.loc -> 'b
val env_get_module : env -> Longident.t Asttypes.loc -> mdl
val env_get_value_or_lvar :
  env -> Longident.t Asttypes.loc -> value_or_lvar
val env_get_constr : env -> Longident.t Asttypes.loc -> int
val env_get_class : env -> Longident.t Asttypes.loc -> class_def
val env_get_module_data :
  env -> Longident.t Asttypes.loc -> mdl_val
