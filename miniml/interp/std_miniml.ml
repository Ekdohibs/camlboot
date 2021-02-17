let assert b = if b = 0 then raise (Assert_failure ("", 0, 0))

let lazy x = x
module Lazy = struct
  let force x = x
end
