let () = print_endline "Functors:"

module F(X : sig val x : int end) = struct
  let x = 2 * X.x
end

module A = F(struct let x = 21 end)
module B = F(struct let x = 12 end)
module X = struct let () = print_string " only once" let x = 16 end
module C = F(X)
module D = F(X)

let () =
  show_int A.x; show_int B.x; if C.x = D.x then print_string " ok" else print_string " ko"

let () = print_newline ()


let () = print_endline "High-order functors:"

module type X = sig val x : int end
module Twice(F : functor (M : X) -> X)(X : X) = struct
  module M = F(F(X))
  let x = M.x
end

module Four = Twice(F)(struct let x = 1 end)
let () =
  show_int Four.x

let () = print_newline ()
