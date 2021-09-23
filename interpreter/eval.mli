open Data

exception Match_fail
val lident_name : Longident.t -> string
val expr_label_shape :
  Parsetree.expression_desc ->
  (Asttypes.arg_label * Parsetree.expression option) list
val fun_label_shape :
  value_ ->
  (Asttypes.arg_label * Parsetree.expression option) list
val mismatch : Location.t -> unit
val unsupported : Location.t -> unit
val take : int -> 'a list -> 'a list
val apply :
  value SMap.t ->
  value ->
  (Asttypes.arg_label * value) list -> value
val eval_expr :
  value SMap.t -> env -> Parsetree.expression -> value
val eval_expr_exn :
  value SMap.t ->
  env -> Parsetree.expression -> (value, value) result
val bind_value :
  value SMap.t -> env -> Parsetree.value_binding -> env
val eval_bindings :
  value SMap.t ->
  env ->
  Asttypes.rec_flag -> Parsetree.value_binding list -> env
val pattern_bind :
  value SMap.t ->
  env -> Parsetree.pattern -> value -> env
val pattern_bind_exn :
  value SMap.t ->
  env -> Parsetree.pattern -> value -> env
val pattern_bind_checkexn :
  value SMap.t ->
  env ->
  Parsetree.pattern -> (value, value) result -> env
val eval_match :
  value SMap.t ->
  env ->
  Parsetree.case list ->
  (value, value) result -> value
val lookup_viewed_object : object_value -> object_value
val eval_expr_in_object :
  value SMap.t ->
  object_value -> expr_in_object -> value
val eval_obj_send :
  Location.t ->
  value SMap.t ->
  object_value -> Asttypes.label Asttypes.loc -> value
val eval_obj_override :
  value SMap.t ->
  env ->
  object_value ->
  (SMap.key Asttypes.loc * Parsetree.expression) list ->
  object_value
val eval_class_expr :
  value SMap.t -> env -> Parsetree.class_expr -> value
val eval_class_structure :
  value SMap.t ->
  env -> Location.t -> Parsetree.class_structure -> object_value
val eval_obj_initializers :
  value SMap.t -> env -> object_value -> unit
val eval_obj_new :
  value SMap.t -> env -> Parsetree.class_expr -> value
val eval_module_expr :
  value SMap.t -> env -> Parsetree.module_expr -> mdl
val eval_functor_data :
  env ->
  Location.t -> mdl -> SMap.key * Parsetree.module_expr * env
val eval_structitem :
  value SMap.t -> env -> Parsetree.structure_item -> env
val eval_structure_ :
  value SMap.t -> env -> Parsetree.structure -> env
val eval_structure :
  value SMap.t -> env -> Parsetree.structure -> env
