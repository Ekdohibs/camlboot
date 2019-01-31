{
  open Parser
  open Lexing
  exception Lexing_error of string

  let kw = [
    "and", AND;
    "begin", BEGIN;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", UIDENT "false";
    "fun", FUN;
    "if", IF;
    "in", IN;
    "let", LET;
    "match", MATCH;
    "module", MODULE;
    "mutable", MUTABLE;
    "of", OF;
    "open", OPEN;
    "rec", REC;
    "struct", STRUCT;
    "then", THEN;
    "true", UIDENT "true";
    "try", TRY;
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
let integer_literal = '-'? digits digits*
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
  | ";;"                        { SEMICOLONSEMICOLON }
  | "="                         { EQ }
  | "|"                         { BAR }
  | "->"                        { MINUSGT }
  | "."                         { DOT }
  | "::"                        { COLONCOLON }
  | ":"                         { COLON }
  | "'"                         { QUOTE }
  | "*"                         { STAR }
  | "+"                         { PLUS }
  | "-"                         { MINUS }
  | "^"                         { CARET }
  | "@"                         { AT }
  | "~"                         { TILDE }
  | "?"                         { QUESTION }
  | "[|"                        { LBRACKBAR }
  | "|]"                        { BARRBRACK }
  | "["                         { LBRACK }
  | "]"                         { RBRACK }
  | "!"                         { BANG }
  | ":="                        { COLONEQ }
  | "<-"                        { LTMINUS }
  | "<"                         { LT }
  | ">"                         { GT }
  | "<="                        { LTEQ }
  | ">="                        { GTEQ }
  | "<>"                        { LTGT }
  | "||"                        { BARBAR }
  | "&&"                        { AMPERAMPER }
  | eof                         { EOF }
  | '\"'                        { STRING (String.concat "" (string_chars lexbuf)) }
  | "'" ([^ '\''] as c) "'"     { INT ("'" ^ String.make 1 c ^ "'") }
  | lident as s                 { try Hashtbl.find keywords s
                                  with Not_found -> LIDENT s }
  | uident as s                 { UIDENT s }
  | integer_literal as s        { INT s }
  | "#"
      {
        let at_beginning_of_line pos = (pos.pos_cnum = pos.pos_bol) in
        if not (at_beginning_of_line lexbuf.lex_start_p)
        then raise (Lexing_error "Illegal caracter: #")
        else try directive lexbuf with Failure _ -> raise (Lexing_error "Illegal caracter: #")
      }
  | _ as c { raise (Lexing_error ("Illegal caracter:" ^ (String.make 1 c))) }

and directive = parse
  | [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '\"' ] * as name) "\"")
        [^ '\010' '\013'] *
      {
        match int_of_string num with
        | exception _ ->
          assert false
        | line_num ->
          let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with pos_lnum = line_num - 1; pos_bol = pos.pos_cnum; pos_fname = name };
          token lexbuf
      }

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
