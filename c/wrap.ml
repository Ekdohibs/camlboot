let parse filename =
  let inc = open_in filename in
  let lexbuf = Lexing.from_channel inc in
  let parsed = Parse.implementation lexbuf in
  close_in inc;
  parsed

let _ = Callback.register "parse" parse