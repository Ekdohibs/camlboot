let () = print_endline "Infix operators treated as sugar:"

let succ n = n + 1
let ignore_and_print_int () n = print_int n
let () = ignore_and_print_int () @@ succ @@ 1
let () = 2 |> succ |> ignore_and_print_int ()

let () = print_newline ()
