open Syntax
open Parser

exception Lexical_error of string * string * int * int
exception Bad_rule

let brace_depth = ref 0

(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html *)

let rec get_next_buff lexbuf n =
  (* Return the next {n} characters in the buffer *)
  let p = lexbuf.Lexing.lex_curr_p in
  let c_pos = p.Lexing.pos_cnum in
  
  (* if there is not enough characters in the buffer, fill it again *)
  if lexbuf.Lexing.lex_buffer_len < n + c_pos
  then begin
    let curr_len = lexbuf.Lexing.lex_buffer_len in
    lexbuf.Lexing.refill_buff lexbuf;
    let new_len = lexbuf.Lexing.lex_buffer_len in
    if curr_len <> new_len
    then get_next_buff lexbuf n
    else if c_pos = curr_len
        then Bytes.of_string ""
        else Bytes.sub lexbuf.Lexing.lex_buffer c_pos (curr_len - c_pos)
  end else
    Bytes.sub lexbuf.Lexing.lex_buffer c_pos n

let advance lexbuf n =
  (* Advance the buffer by {n} characters *)
  let p = lexbuf.Lexing.lex_curr_p in
  let cnum, lnum, bol = ref p.Lexing.pos_cnum, ref p.Lexing.pos_lnum, ref p.Lexing.pos_bol in
  let buff = get_next_buff lexbuf n in
  for i = 0 to (Bytes.length buff - 1) do
    if (Bytes.get buff i) = '\010'
    then begin incr lnum; bol := !cnum end;
    incr cnum
  done;
  lexbuf.Lexing.lex_curr_p <- { p with
        Lexing.pos_cnum = !cnum;
        Lexing.pos_lnum = !lnum;
        Lexing.pos_bol = !bol;
  }

let lex_str lexbuf str =
  let buf = get_next_buff lexbuf (String.length str) in

  for i = 0 to (String.length str - 1) do
    if (Bytes.length buf) <= i then raise Bad_rule;
    if (String.get str i) <> (Bytes.get buf i)
    then raise Bad_rule;
  done;

  advance lexbuf (String.length str)

let rec lex_action lexbuf =
  let buff = get_next_buff lexbuf 200 in
  let i = ref 0 in
  while (!brace_depth > 0) && (!i < (Bytes.length buff)) do
    begin match (Bytes.get buff !i) with
    | '{' -> incr brace_depth
    | '}' -> decr brace_depth
    | _ -> ()
    end;
    incr i
  done;
  advance lexbuf !i;
  if !brace_depth <> 0
  then lex_action lexbuf
;;

let rec lex_string lexbuf =
  let buff = get_next_buff lexbuf 200 in
  let i = ref 0 in
  let continue = ref true in
  let str = ref [] in
  while (!continue = true) && (!i < Bytes.length buff) do
    begin match Bytes.get buff !i with
    | '\\' -> begin match Bytes.get buff (!i+1) with
      | '\\' -> str := '\\' :: !str
      | '\'' -> str := '\'' :: !str
      | '"' -> str := '"' :: !str
      | 'n' -> str := '\010' :: !str
      | 't' -> str := '\009' :: !str
      | 'b' -> str := '\008' :: !str
      | 'r' -> str := '\013' :: !str
      | ' ' -> str := ' ' :: !str
      (* We skip many cases that do not happen in lexer.mll, such as \\[0-9]{3},
       * or \\ before a new line. *)
      | _ -> raise Bad_rule
      end; incr i
    | '"' -> continue := false
    | c -> str := c :: !str
    end;
    incr i
  done;
  advance lexbuf !i;
  let buf = Buffer.create (List.length !str) in
  List.iter (Buffer.add_char buf) !str;
  if !continue = true
  then (Buffer.contents buf)::(lex_string lexbuf)
  else (Buffer.contents buf)::[];;

let lex_comment lexbuf =
  let buff = get_next_buff lexbuf 200 in
  let star = ref false in
  let i = ref 0 in
  let continue = ref true in
  while !continue do
    begin match (Bytes.get buff !i) with
    | '*' -> star := true
    | ')' -> if !star then continue := false
    | _ -> star := false
    end;
    incr i
  done;
  advance lexbuf !i
;;

let lex_ident lexbuf =
  let buff = get_next_buff lexbuf 100 in
  if (Bytes.length buff) = 0 then raise Bad_rule;
  let first = Bytes.get buff 0 in
  (* ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'] *)
  if (('A' <= first) && (first <= 'Z')) || (('a' <= first) && (first <= 'z'))
        || (first = '_') || (('\192' <= first) && (first <= '\214'))
        || (('\216' <= first) && (first <= '\246'))
        || (('\248' <= first) && (first <= '\255'))
  then begin
          let continue = ref true in
          let i = ref 1 in
          while !continue do
            (* ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9'] *)
            let chr = Bytes.get buff !i in
            if (('A' <= chr) && (chr <= 'Z')) || (('a' <= chr) && (chr <= 'z'))
                || (chr = '_') || (('\192' <= chr) && (chr <= '\214'))
                || (('\216' <= chr) && (chr <= '\246'))
                || (('\248' <= chr) && (chr <= '\255')) || (chr = '\'')
                || (('0' <= chr) && (chr <= '9'))
            then incr i
            else continue := false
          done;
          advance lexbuf !i;
          Bytes.to_string (Bytes.sub buff 0 !i)
  end else raise Bad_rule

let decimal_code  c d u =
  100 * (Char.code c - Char.code '0')
    + 10 * (Char.code d - Char.code '0')
    + (Char.code u - Char.code '0')

let lex_char lexbuf =
  let buff = get_next_buff lexbuf 6 in
  if (Bytes.length buff) = 0 then raise Bad_rule;
  let first = Bytes.get buff 0 in
  if first = '\''
  then begin
    match Bytes.get buff 1 with
    | '\\' -> begin match Bytes.get buff 2 with
              | '\\' -> begin advance lexbuf 4; Tchar (Char.code('\\')) end
              | '\'' -> begin advance lexbuf 4; Tchar (Char.code('\'')) end
              | '"' ->  begin advance lexbuf 4; Tchar (Char.code('"')) end
              | ' ' ->  begin advance lexbuf 4; Tchar (Char.code(' ')) end
              | 'r' ->  begin advance lexbuf 4; Tchar (Char.code('\013')) end
              | 'b' ->  begin advance lexbuf 4; Tchar (Char.code('\008')) end
              | 'n' ->  begin advance lexbuf 4; Tchar (Char.code('\010')) end
              | 't' ->  begin advance lexbuf 4; Tchar (Char.code('\009')) end
              (* We should check that we actually have that many chars left,
               * and that they are numbers. Fortunately, lexer.mll is
               * well-formed. *)
              | _ ->    begin advance lexbuf 6;
                Tchar (decimal_code (Bytes.get buff 2) (Bytes.get buff 3)
                                    (Bytes.get buff 4)) end
              end
    | c -> if (Bytes.get buff 2) = '\''
           then begin advance lexbuf 3; Tchar (Char.code(c))
           end else raise Bad_rule
  end else raise Bad_rule


let skip_space_and_comments lexbuf =
  let str = get_next_buff lexbuf 2 in
  let len = Bytes.length str in
  if len = 0 then raise Bad_rule
  else match (Bytes.get str 0) with
  | '(' -> if (len > 1) && ((Bytes.get str 1) = '*')
           then begin
             advance lexbuf 2;
             lex_comment lexbuf;
           end else
             raise Bad_rule
  | ' ' -> advance lexbuf 1
  | '\009' -> advance lexbuf 1
  | '\010' -> advance lexbuf 1
  | '\012' -> advance lexbuf 1
  | '\013' -> advance lexbuf 1
  | _ -> raise Bad_rule

let rec try_skip_space_and_comments lexbuf =
  match skip_space_and_comments lexbuf with
  | () -> try_skip_space_and_comments lexbuf
  | exception Bad_rule -> ()

let _main lexbuf: Parser.token =
  (* 1. skip spaces and comments *)
  try_skip_space_and_comments lexbuf;
  (* 2. try to parse each type *)
  (* Note that these would have been incorrect if we had ident such as "letrec"
   * which start with a keyword, but are not keywords.  Thanksfully, we need
   * to parse only the lexer's lexer, which doesn't contain any. *)
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  try lex_str lexbuf "rule"; Trule
  with Bad_rule ->
  try lex_str lexbuf "parse"; Tparse
  with Bad_rule ->
  try lex_str lexbuf "and"; Tand
  with Bad_rule ->
  try lex_str lexbuf "let"; Tlet
  with Bad_rule ->
  try lex_str lexbuf "eof"; Teof
  with Bad_rule ->
  try lex_str lexbuf "as"; Tas
  with Bad_rule ->
  (* No identifier in lexer.mll starts with a _, so we can match it simply here *)
  try lex_str lexbuf "_"; Tunderscore
  with Bad_rule ->
  try Tident (lex_ident lexbuf)
  with Bad_rule ->
  try lex_char lexbuf
  with Bad_rule ->
  try lex_str lexbuf "="; Tequal
  with Bad_rule ->
  try lex_str lexbuf "|"; Tor
  with Bad_rule ->
  try lex_str lexbuf "["; Tlbracket
  with Bad_rule ->
  try lex_str lexbuf "]"; Trbracket
  with Bad_rule ->
  try lex_str lexbuf "*"; Tstar
  with Bad_rule ->
  try lex_str lexbuf "?"; Tmaybe
  with Bad_rule ->
  try lex_str lexbuf "+"; Tplus
  with Bad_rule ->
  try lex_str lexbuf "("; Tlparen
  with Bad_rule ->
  try lex_str lexbuf ")"; Trparen
  with Bad_rule ->
  try lex_str lexbuf "^"; Tcaret
  with Bad_rule ->
  try lex_str lexbuf "-"; Tdash
  with Bad_rule ->
  (* We skip lines such as "# 18 "lexer.mll"" as they do not appear in
   * lexer.mll. *)
  try lex_str lexbuf "#"; Thash
  with Bad_rule ->
  try lex_str lexbuf "{"; brace_depth := 1;
    let old_pos = lexbuf.Lexing.lex_curr_p in
    lex_action lexbuf;
    let new_pos = lexbuf.Lexing.lex_curr_p in
    Taction ({loc_file = old_pos.Lexing.pos_fname; start_pos = old_pos.Lexing.pos_cnum;
      end_pos = new_pos.Lexing.pos_cnum-1; start_line = old_pos.Lexing.pos_lnum;
      start_col = old_pos.Lexing.pos_cnum - old_pos.Lexing.pos_bol})
  with Bad_rule ->
  try lex_str lexbuf "\""; Tstring (String.concat "" (lex_string lexbuf))
  with Bad_rule ->
  if (Bytes.length (get_next_buff lexbuf 1)) = 0
  then Tend else raise (Lexical_error ("","",0,0))

let loc_to_str loc =
  Printf.sprintf "file: %s, start_pos: %d, end_pos: %d, start_line: %d, start_col: %d"
    loc.loc_file loc.start_pos loc.end_pos loc.start_line loc.start_col

let print_token tokk =
match tokk with
  | Tident str -> Printf.printf "Tident (%s)\n" str
  | Tchar i -> Printf.printf "Tchar (%d)\n" i
  | Tstring str -> Printf.printf "Tstring (%s)\n" str
  | Taction loc -> Printf.printf "Taction (%s)\n" (loc_to_str loc)
  | Trule -> Printf.printf "Trule\n"
  | Tparse -> Printf.printf "Tparse\n"
  | Tparse_shortest -> Printf.printf "Tparse_shortest\n"
  | Tand -> Printf.printf "Tand\n"
  | Tequal -> Printf.printf "Tequal\n"
  | Tend -> Printf.printf "Tend\n"
  | Tor -> Printf.printf "Tor\n"
  | Tunderscore -> Printf.printf "Tunderscore\n"
  | Teof -> Printf.printf "Teof\n"
  | Tlbracket -> Printf.printf "Tlbracket\n"
  | Trbracket -> Printf.printf "Trbracket\n"
  | Trefill -> Printf.printf "Trefill\n"
  | Tstar -> Printf.printf "Tstar\n"
  | Tmaybe -> Printf.printf "Tmaybe\n"
  | Tplus -> Printf.printf "Tplus\n"
  | Tlparen -> Printf.printf "Tlparen\n"
  | Trparen -> Printf.printf "Trparen\n"
  | Tcaret -> Printf.printf "Tcaret\n"
  | Tdash -> Printf.printf "Tdash\n"
  | Tlet -> Printf.printf "Tlet\n"
  | Tas -> Printf.printf "Tas\n"
  | Thash -> Printf.printf "Thash\n"

let main lexbuf =
  let tokk = _main lexbuf in
  print_token tokk;
  Printf.printf "%!";
  (*print_lexbuf lexbuf;*)
  tokk;;
