{
  open Parser
  open Lexing
  exception Lexing_error of string

  let kw = [
    "else", ELSE;
    "if", IF;
    "in", IN;
    "let", LET;
    "match", MATCH;
    "of", OF;
    "then", THEN;
    "type", TYPE;
    "with", WITH
  ]
  
  let keywords = Hashtbl.create (List.length kw)
  let () = List.iter (fun (a,b) -> Hashtbl.add keywords a b) kw

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }  
}

let digits = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let hex_digit = digits | ['A'-'F'] | ['a'-'f']
let ident_car = alpha | '_' | '\'' | digits
let lident = (['a'-'z'] | '_') ident_car*
let uident = ['A'-'Z'] ident_car*
let integer_literal = '-'? digits (digits | '_')*
let escape_sequence = '\\'
   ('\\' | '\"' | '\'' | 'n' | 't' | 'b' | 'r' | ' ' |
    (digits digits digits) | ('x' hex_digit hex_digit))

let whitespace =  [ ' ' '\t' ]

rule token = parse
  | whitespace+                 { token lexbuf }
  | "(*"                        { comment lexbuf ; token lexbuf }
  | "\n"                        { newline lexbuf ; token lexbuf }
  | "("                         { LPAREN }
  | ")"                         { RPAREN }
  | "{"                         { LBRACE }
  | "}"                         { RBRACE }
  | ","                         { COMMA }
  | ";"                         { SEMICOLON }
  | "="                         { EQ }
  | "|"                         { BAR }
  | "->"                        { MINUSGT }
  | "."                         { DOT }
  | eof                         { EOF }
  | '\"'                        { STRING (String.concat "" (string_chars lexbuf)) }
  | lident as s                 { try Hashtbl.find keywords s
                                  with Not_found -> LIDENT s }
  | uident as s                 { UIDENT s }
  | _ as c { raise (Lexing_error ("Illegal caracter:" ^ (String.make 1 c))) }

and comment = parse
  | "\n"    { newline lexbuf; comment lexbuf }
  | "*)"    { () }
  | "(*"    { comment lexbuf; comment lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error "Unterminated comment") }
     
and string_chars = parse
  | '\"' { [] }
  | '\n' { newline lexbuf; "\n" :: (string_chars lexbuf) }
  | [^ '\\' '\"'] as c { (String.make 1 c) :: (string_chars lexbuf)}
  | escape_sequence as s { s :: (string_chars lexbuf) }
  | '\\' { string_escape_newline lexbuf; string_chars lexbuf }
  | _ { raise (Lexing_error "Unrecognized escape sequence") }
     
and string_escape_newline = parse
  | '\n' { newline lexbuf; string_skip_indent lexbuf }         

and string_skip_indent = parse
  | [' ' '\t']* { }
