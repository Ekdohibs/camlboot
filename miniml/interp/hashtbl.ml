external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash"

let find t x = List.assoc x t
let mem t x = List.mem_assoc x t
let hash x = seeded_hash_param 10 100 0 x
