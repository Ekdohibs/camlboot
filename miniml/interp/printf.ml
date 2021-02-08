let printf fmt = Format.(mkprintf false { out_string = print_string } fmt (fun () -> ()))
let fprintf ff fmt = Format.(mkprintf false ff fmt (fun () -> ()))
let eprintf fmt = Format.(mkprintf false { out_string = print_err } fmt (fun () -> ()))
let kbprintf k b fmt = Format.(mkprintf false { out_string = Buffer.add_string b } fmt (fun () -> k b))
let bprintf b fmt = kbprintf (fun _ -> ()) b fmt
let kprintf k fmt = kbprintf (fun b -> k (Buffer.contents b)) (Buffer.create 16) fmt
let ksprintf = kprintf
let kasprintf = kprintf
let sprintf fmt = kprintf (fun s -> s) fmt
