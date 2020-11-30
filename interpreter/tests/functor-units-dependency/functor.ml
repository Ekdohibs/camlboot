module F(X : sig val x : int end) = struct
  let x = X.x
end
