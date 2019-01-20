%{
  open Ast
%}

%token LPAREN
%token RPAREN
%token COMMA
%token MINUSGT
%token DOT
%token SEMICOLON
%token EQ
%token LBRACE
%token BAR
%token RBRACE
%token COLON
%token COLONCOLON
%token QUOTE
%token STAR
%token CARET
%token AT
%token TILDE
%token QUESTION
%token SEMICOLONSEMICOLON

%token ELSE
%token IF
%token IN
%token LET
%token MATCH
%token OF
%token THEN
%token TYPE
%token WITH

%token EOF
%token <string> STRING
%token <string> LIDENT
%token <string> UIDENT
%token <string> INT

%right MINUSGT
%left BAR
%nonassoc annot_prec
%nonassoc LET MATCH
%right SEMICOLON
%right list_prec
%nonassoc THEN
%nonassoc ELSE
%nonassoc comma_prec
%left COMMA
%left EQ
%right CARET AT
%right COLONCOLON
%nonassoc label_prec
%nonassoc COLON
%nonassoc dot_prec
%nonassoc DOT
%nonassoc RPAREN


%start definitions

%type <Ast.definition list> definitions

%%

list2(X):
  | x1 = X; x2 = X { [x1; x2] }
  | x = X; l = list2(X) { x :: l }

separated_list2(DELIM, X):
  | x1 = X; DELIM; x2 = X { [x1; x2] }
  | x = X; DELIM; l = separated_list2(DELIM, X) { x :: l }

comma_separated_list2_(X):
  | x1 = X; COMMA; x2 = X { [x2; x1] }
  | l = comma_separated_list2_(X); COMMA; x = X { x :: l }

%inline comma_separated_list2(X):
  l = comma_separated_list2_(X) { List.rev l }

separated_semi_opt(X):
  | x = X; ioption(SEMICOLON) { [x] }
  | x = X; SEMICOLON; l = separated_semi_opt(X) { x :: l }

type_ignore:
  | { () }
  | STAR; type_ignore { () }
  | COMMA; type_ignore { () }
  | MINUSGT; type_ignore { () }
  | QUOTE; type_ignore { () }
  | LIDENT; type_ignore { () }
  | UIDENT; type_ignore { () }
  | DOT; type_ignore { () }
  | LPAREN; type_ignore; RPAREN; type_ignore { () }

constant:
  | s = STRING { CString s }
  | LPAREN; RPAREN { CUnit }
  | i = INT { CInt i }

record_list_(X):
  | f = LIDENT; EQ; x = X %prec list_prec { [(f, x)] }
  | l = record_list_(X); SEMICOLON; f = LIDENT; EQ; x = X %prec list_prec
      { (f, x) :: l }

%inline record_list(X):
  l = record_list_(X) { List.rev l }

record(X):
  LBRACE; l = record_list(X); option(SEMICOLON); RBRACE { l }

pattern:
  | x = LIDENT { PVar x }
  | c = UIDENT { PConstructor (c, []) }
  | c = UIDENT x = LIDENT { PConstructor (c, [x]) }
  | c = UIDENT LPAREN l = separated_nonempty_list(COMMA, LIDENT) RPAREN
    { PConstructor (c, l) }
  | l = comma_separated_list2(LIDENT) { PConstructor ("", l) }
  | x = LIDENT COLONCOLON y = LIDENT { PConstructor ("Cons", [x; y]) }

simple_expr:
  | v = LIDENT { EVar v }
  | c = constant { EConstant c }
  | c = UIDENT %prec dot_prec
      { EConstr (c, []) }
  | LPAREN; e = expr; RPAREN { e }
  | LPAREN; e = expr; COLON; type_ignore; RPAREN { e }
  | e = simple_expr; DOT; f = LIDENT
      { EGetfield (e, f) }
  | l = record(expr)
      { ERecord l }
  | LBRACE; e = simple_expr; WITH; l = record_list(expr); RBRACE
      { ERecordwith (e, l) }

labelled_simple_expr:
  | e = simple_expr { (e, Nolabel) }
  | TILDE; l = LIDENT %prec label_prec { (EVar l, Labelled l) }
  | QUESTION; l = LIDENT %prec label_prec { (EVar l, Optional l) }
  | TILDE; l = LIDENT; COLON; e = simple_expr { (e, Labelled l) }
  | QUESTION; l = LIDENT; COLON; e = simple_expr { (e, Optional l) }

expr:
  | e = simple_expr { e }
  | f = LIDENT; l = nonempty_list(labelled_simple_expr) (* %prec appl_prec *)
      { EApply (f, l) }
  | l = comma_separated_list2(expr) %prec comma_prec
      { EConstr ("", l) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
      { EIf (e1, e2, e3) }
  | IF; e1 = expr; THEN; e2 = expr
      { EIf (e1, e2, EConstant CUnit) }
  | e1 = expr; SEMICOLON; e2 = expr
      { EChain (e1, e2) }
  | MATCH; e = expr; WITH; p = pattern_matching (* %prec MATCH *)
      { EMatch (e, p) }
  | LET; p = pattern; EQ; e1 = expr; IN; e2 = expr %prec LET
      { ELet (p, e1, e2) }
  | e1 = expr; COLONCOLON; e2 = expr
      { EConstr ("Cons", [e1; e2])}
  | e1 = expr; CARET; e2 = expr
      { EApply ("string_concat", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; AT; e2 = expr
      { EApply ("list_concat", [(e1, Nolabel); (e2, Nolabel)]) }

%inline pattern_line:
  p = pattern; MINUSGT; e = expr
     { (p, e) }

pattern_lines:
  | p = pattern_line { [p] }
  | p = pattern_line; BAR; l = pattern_lines { p :: l }

%inline pattern_matching:
  ioption(BAR); l = pattern_lines { l }

field_decl:
  | f = LIDENT; COLON; type_ignore { f }

%inline type_representation:
  | ioption(BAR); l = separated_nonempty_list(BAR, constr_decl)
    { (ISum l) }
  | LBRACE; l = separated_semi_opt(field_decl); RBRACE
    { (IRecord l) }

constr_decl:
  | n = UIDENT { (n, false) }
  | n = UIDENT; OF; type_ignore { (n, true) }

labelled_args:
  | x = LIDENT { (x, Nolabel) }
  | TILDE; x = LIDENT { (x, Labelled x) }
  | QUESTION; x = LIDENT { (x, Optional x) }

definition:
  | LET; name = LIDENT; vars = list(labelled_args); EQ; body = expr { MLet (name, vars, body) }
  | TYPE; n = LIDENT; EQ; r = type_representation { MTypedef (n, r) }

semidefinition:
  | ioption(SEMICOLONSEMICOLON); d = definition { d }

definitions:
  | l = list(semidefinition); EOF { l }
