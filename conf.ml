let trace = false
let tracearg_from = 742740000
let tracecur = ref 0

let debug =
  match Sys.getenv_opt "OCAMLINTERP_DEBUG" with
  | Some ("1" | "true" | "yes") -> true
  | Some ("0" | "false" | "no") -> false
  | Some other ->
    Printf.kprintf
      failwith
      "Error: unknown OCAMLINTERP_DEBUG value %S, use 'true' or 'false'"
      other
  | None ->
    (* default *)
    false

let stdlib_path () =
  match Sys.getenv_opt "OCAMLINTERP_STDLIB_PATH" with
  | Some path -> path
  | None ->
    let input = Unix.open_process_in "ocamlc -where" in
    (match input_line input with
    | exception _ ->
      close_in input;
      failwith "Error: unable to determine the standard library location"
    | path ->
      close_in input;
      path)

let compiler_source_path () =
  match Sys.getenv_opt "OCAMLINTERP_SRC_PATH" with
  | Some path -> path
  | None ->
    failwith
      "Error: please set an OCAMLINTERP_SRC_PATH variable pointing to a \
       checkout of the OCaml compiler distribution sources"
