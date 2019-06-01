open Data

let prim1 f unwrap1 wrap = Prim (fun x -> wrap (f (unwrap1 x)))

let prim2 f unwrap1 unwrap2 wrap =
  Prim (fun x -> prim1 (f (unwrap1 x)) unwrap2 wrap)

let prim3 f unwrap1 unwrap2 unwrap3 wrap =
  Prim (fun x -> prim2 (f (unwrap1 x)) unwrap2 unwrap3 wrap)

let prim4 f unwrap1 unwrap2 unwrap3 unwrap4 wrap =
  Prim (fun x -> prim3 (f (unwrap1 x)) unwrap2 unwrap3 unwrap4 wrap)

let prim5 f unwrap1 unwrap2 unwrap3 unwrap4 unwrap5 wrap =
  Prim (fun x -> prim4 (f (unwrap1 x)) unwrap2 unwrap3 unwrap4 unwrap5 wrap)

let id x = x

let apply_ref =
  ref
    (fun _ _ -> assert false
      : value -> (Asttypes.arg_label * value) list -> value)
