exception Fatal_error
val fatal_error : 'a -> 'b
val create_hashtable : int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
val may : ('a -> unit) -> 'a option -> unit
 