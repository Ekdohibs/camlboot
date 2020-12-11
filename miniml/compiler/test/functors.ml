let () = print_endline "Functors:"

module F(X : sig val x : int end) = struct
  let x = 2 * X.x
end

module A = F(struct let x = 21 end)
module B = F(struct let x = 12 end)
module X = struct let () = print " only once" let x = 16 end
module C = F(X)
module D = F(X)

let () =
  print_int A.x; print_int B.x; if C.x = D.x then print " ok" else print " ko"

let () = print_newline ()
