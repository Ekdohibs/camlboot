let () = print_endline "Infix operators treated as sugar:"

let succ n = n + 1
let ignore_and_show_int () n = show_int n
let () = ignore_and_show_int () @@ succ @@ 1
let () = 2 |> succ |> ignore_and_show_int ()

let () = print_newline ()
