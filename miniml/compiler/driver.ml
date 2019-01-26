open Lexing

let input_file = Sys.argv.(1)
let in_chan = open_in input_file
let lexbuf = Lexing.from_channel in_chan

let report_error start_pos end_pos =
  let filename = start_pos.pos_fname in
  let start_col = start_pos.pos_cnum - start_pos.pos_bol + 1 in
  let end_col = end_pos.pos_cnum - start_pos.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" filename start_pos.pos_lnum start_col end_col

let defs =
  try Parser.definitions Lexer.token lexbuf
  with
  | Parser.Error -> begin
      report_error (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf);
      Format.eprintf "Syntax error@."; exit 1
    end
  | Lexer.Lexing_error s -> begin
      report_error (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf);
      Format.eprintf "Lexing error: %s@." s; exit 1
    end

let () = Compile.compile_and_print Format.std_formatter defs
