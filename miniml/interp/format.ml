
type formatter_functions = {
  out_string : string -> unit
}

let error msg =
  print_string msg; print_string "\n"; failwith msg

let mkprintf is_format print_fun ff fmt cont =
  let out_string = print_fun ff in
  let rec loop i =
    let j = ref i in
    while !j < String.length fmt && fmt.[!j] <> '%' && (not is_format || fmt.[!j] <> '@') do
      incr j
    done;
    let j = !j in
    if (i < j) then
      out_string (String.sub fmt i (j - i));
    if j < String.length fmt then begin
      if (fmt.[j] = '%') then begin
        assert (j + 1 < String.length fmt);
        match fmt.[j + 1] with
        | 'd' | 'i' -> Obj.magic (fun n -> (out_string (string_of_int n); loop (j + 2)))
        | 'L' ->
            assert (j + 2 < String.length fmt && (fmt.[j + 2] = 'd' || fmt.[j + 2] = 'i'));
            Obj.magic (fun n -> out_string (Int64.to_string n); loop (j + 3));
        | 'n' ->
            assert (j + 2 < String.length fmt && (fmt.[j + 2] = 'd' || fmt.[j + 2] = 'i'));
            Obj.magic (fun n -> out_string (Nativeint.to_string n); loop (j + 3));
        | 'f' -> Obj.magic (fun f -> (out_string (format_float "%f" f); loop (j + 2))) 
        | 's' -> Obj.magic (fun s -> (out_string s; loop (j + 2)))
        | 'S' -> Obj.magic (fun s -> (out_string (String.escaped s); loop (j + 2)))
        | 'a' -> Obj.magic (fun f x -> f ff x; loop (j + 2))
        | '%' -> out_string "%"; loop (j + 2)
        | _ -> error ("Unknown format specifier: " ^ String.sub fmt j 2)
      end else begin
        assert (fmt.[j] = '@' && is_format);
        assert (j + 1 < String.length fmt);
        match fmt.[j + 1] with
        | '@' -> out_string "@"; loop (j + 2)
        | '.' -> out_string "\n"; loop (j + 2)
        | _ -> error ("Unknown format specifier: " ^ String.sub fmt j 2)
      end
    end else begin
      cont ()
    end
  in
  loop 0

let getff ff = ff.out_string
let printf fmt = mkprintf true getff { out_string = print_string } fmt (fun () -> ())
let fprintf ff fmt = mkprintf true getff ff fmt (fun () -> ())
let eprintf fmt = mkprintf true getff { out_string = print_err } fmt (fun () -> ())
let kbprintf k b fmt = mkprintf true getff { out_string = Buffer.add_string b } fmt (fun () -> k b)
let bprintf b fmt = kbprintf (fun _ -> ()) b fmt
let kprintf k fmt = kbprintf (fun b -> k (Buffer.contents b)) (Buffer.create 16) fmt
let ksprintf = kprintf
let kasprintf = kprintf
let sprintf fmt = kprintf (fun s -> s) fmt

let pp_print_cut ff () = fprintf ff " "
let pp_print_list ?(pp_sep = pp_print_cut) f ff l =
  match l with
  | [] -> ()
  | a :: l -> f ff a; List.iter (fun a -> pp_sep ff (); f ff a) l
