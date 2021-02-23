open Data

val cc : string -> int -> value
external reraise : exn -> 'a = "%reraise"
val builtin_exn_handler : (exn -> value option) -> (unit -> 'a) -> 'a
val prim1 :
  ('a -> 'b) ->
  (exn -> value option) ->
  (value -> 'a) -> ('b -> value) -> value
val prim2 :
  ('a -> 'b -> 'c) ->
  (exn -> value option) ->
  (value -> 'a) ->
  (value -> 'b) -> ('c -> value) -> value
val prim3 :
  ('a -> 'b -> 'c -> 'd) ->
  (exn -> value option) ->
  (value -> 'a) ->
  (value -> 'b) ->
  (value -> 'c) -> ('d -> value) -> value
val prim4 :
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  (exn -> value option) ->
  (value -> 'a) ->
  (value -> 'b) ->
  (value -> 'c) ->
  (value -> 'd) -> ('e -> value) -> value
val prim5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  (exn -> value option) ->
  (value -> 'a) ->
  (value -> 'b) ->
  (value -> 'c) ->
  (value -> 'd) ->
  (value -> 'e) -> ('f -> value) -> value
val id : 'a -> 'a
val builtin_exn_id : env -> string -> int
val exn0 : env -> string -> value
val exn1 :
  env -> string -> ('a -> value) -> 'a -> value
val exn2 :
  env ->
  string ->
  ('a -> value) ->
  ('b -> value) -> 'a -> 'b -> value
val exn3 :
  env ->
  string ->
  ('a -> value) ->
  ('b -> value) ->
  ('c -> value) -> 'a -> 'b -> 'c -> value
