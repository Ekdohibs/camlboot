let getff ff = ff.Format.out_string
let getoc oc = output_string oc

let printf fmt = Format.(mkprintf false getoc stdout fmt (fun () -> ()))
let fprintf ff fmt = Format.(mkprintf false getoc ff fmt (fun () -> ()))
let eprintf fmt = Format.(mkprintf false getoc stderr fmt (fun () -> ()))

let kbprintf k b fmt = Format.(mkprintf false getff { out_string = Buffer.add_string b } fmt (fun () -> k b))
let bprintf b fmt = kbprintf (fun _ -> ()) b fmt
let kprintf k fmt = kbprintf (fun b -> k (Buffer.contents b)) (Buffer.create 16) fmt
let ksprintf = kprintf
let kasprintf = kprintf
let sprintf fmt = kprintf (fun s -> s) fmt
