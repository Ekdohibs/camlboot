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
%token LBRACK
%token RBRACK
%token LBRACKBAR
%token BARRBRACK
%token BANG
%token COLONEQ
%token LTMINUS
%token PLUS
%token MINUS
%token LT
%token LTEQ
%token GT
%token GTEQ
%token LTGT
%token BARBAR
%token AMPERAMPER

%token AND
%token BEGIN
%token ELSE
%token END
%token EXCEPTION
%token EXTERNAL
%token FUN
%token IF
%token IN
%token LET
%token MATCH
%token MODULE
%token MUTABLE
%token OF
%token OPEN
%token REC
%token STRUCT
%token THEN
%token TRY
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
%nonassoc LTMINUS
%right COLONEQ
%nonassoc comma_prec
%left COMMA
%right BARBAR
%right AMPERAMPER
%left EQ LTGT LT GT LTEQ GTEQ
%right CARET AT
%right COLONCOLON
%left PLUS MINUS
%left STAR
%nonassoc label_prec
%nonassoc COLON
%nonassoc dot_prec
%nonassoc DOT
%nonassoc RPAREN
%nonassoc BANG

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
  | longident_lident; type_ignore { () }
  | LPAREN; type_ignore; RPAREN; type_ignore { () }

constant:
  | s = STRING { CString s }
  | LPAREN; RPAREN { CUnit }
  | BEGIN; END { CUnit }
  | i = INT { CInt i }

longident_uident:
  | x = UIDENT { Lident x }
  | l = longident_uident; DOT; x = UIDENT { Ldot (l, x) }

longident_constr:
  | x = longident_uident %prec dot_prec { x }

longident_lident:
  | x = LIDENT { Lident x }
  | l = longident_uident; DOT; x = LIDENT { Ldot (l, x) }


record_list_(X):
  | f = longident_lident; EQ; x = X %prec list_prec { [(f, x)] }
  | l = record_list_(X); SEMICOLON; f = longident_lident; EQ; x = X %prec list_prec
      { (f, x) :: l }

%inline record_list(X):
  l = record_list_(X) { List.rev l }

record(X):
  LBRACE; l = record_list(X); option(SEMICOLON); RBRACE { l }

pattern:
  | x = LIDENT { PVar x }
  | c = longident_constr { PConstructor (c, []) }
  | c = longident_constr x = LIDENT { PConstructor (c, [x]) }
  | c = longident_constr LPAREN l = separated_nonempty_list(COMMA, LIDENT) RPAREN
    { PConstructor (c, l) }
  | l = comma_separated_list2(LIDENT) { PConstructor (Lident "", l) }
  | LBRACK; RBRACK { PConstructor (Lident "Null", []) }
  | x = LIDENT COLONCOLON y = LIDENT { PConstructor (Lident "Cons", [x; y]) }
  | LPAREN; p = pattern; COLON; type_ignore; RPAREN { p }
  | LPAREN; RPAREN { PInt "0" }
  | LPAREN; p = pattern; RPAREN { p }
  | i = INT { PInt i }

simple_expr:
  | v = longident_lident { EVar v }
  | c = constant { EConstant c }
  | c = longident_constr %prec dot_prec
      { EConstr (c, []) }
  | LPAREN; e = expr; RPAREN { e }
  | BEGIN; e = expr; END { e }
  | LPAREN; e = expr; COLON; type_ignore; RPAREN { e }
  | e = simple_expr; DOT; f = longident_lident
      { EGetfield (e, f) }
  | l = record(expr)
      { ERecord l }
  | LBRACE; e = simple_expr; WITH; l = record_list(expr); ioption(SEMICOLON); RBRACE
      { ERecordwith (e, l) }
  | LBRACK; RBRACK { EConstr (Lident "Null", []) }
  | LBRACK; l = semi_separated_expr_list_opt; RBRACK { List.fold_right (fun e r -> EConstr (Lident "Cons", [e; r])) l (EConstr (Lident "Null", [])) }
  | LBRACKBAR; l = semi_separated_expr_list_opt; BARRBRACK { EConstr (Lident "", l) }
  | BANG; e = simple_expr
      { EApply (Lident "ref_get", [(e, Nolabel)]) }
  | e1 = simple_expr; DOT; LPAREN; e2 = expr; RPAREN
      { EApply (Lident "array_get", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = simple_expr; DOT; LBRACK; e2 = expr; RBRACK
      { EApply (Lident "string_get", [(e1, Nolabel); (e2, Nolabel)]) }

labelled_simple_expr:
  | e = simple_expr { (e, Nolabel) }
  | TILDE; l = LIDENT %prec label_prec { (EVar (Lident l), Labelled l) }
  | QUESTION; l = LIDENT %prec label_prec { (EVar (Lident l), Optional l) }
  | TILDE; l = LIDENT; COLON; e = simple_expr { (e, Labelled l) }
  | QUESTION; l = LIDENT; COLON; e = simple_expr { (e, Optional l) }

expr:
  | e = simple_expr { e }
  | FUN; l = nonempty_list(LIDENT); MINUSGT; e = expr { ELambda (l, e) }
  | f = longident_lident; l = nonempty_list(labelled_simple_expr) (* %prec appl_prec *)
      { EApply (f, l) }
  | c = longident_constr; e = simple_expr { EConstr (c, [e]) }
  | l = comma_separated_list2(expr) %prec comma_prec
      { EConstr (Lident "", l) }
  | e1 = simple_expr; DOT; f = longident_lident; LTMINUS; e2 = expr;
      { ESetfield (e1, f, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
      { EIf (e1, e2, e3) }
  | IF; e1 = expr; THEN; e2 = expr
      { EIf (e1, e2, EConstant CUnit) }
  | e1 = expr; SEMICOLON; e2 = expr
      { EChain (e1, e2) }
  | e1 = expr; EQ; e2 = expr
      { EApply (Lident "eq", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; LTGT; e2 = expr
      { EApply (Lident "neq", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; LT; e2 = expr
      { EApply (Lident "lessthan", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; GT; e2 = expr
      { EApply (Lident "lessthan", [(e2, Nolabel); (e1, Nolabel)]) }
  | e1 = expr; LTEQ; e2 = expr
      { EApply (Lident "lessequal", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; GTEQ; e2 = expr
      { EApply (Lident "lessequal", [(e2, Nolabel); (e1, Nolabel)]) }
  | e1 = expr; PLUS; e2 = expr
      { EApply (Lident "plus", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; MINUS; e2 = expr
      { EApply (Lident "minus", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; AMPERAMPER; e2 = expr
      { EIf (e1, e2, EConstant (CInt "0")) }
  | e1 = expr; BARBAR; e2 = expr
      { EIf (e1, EConstant (CInt "1"), e2) }
  | e1 = expr; STAR; e2 = expr
      { EApply (Lident "times", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; COLONEQ; e2 = expr
      { EApply (Lident "ref_set", [(e1, Nolabel); (e2, Nolabel)]) }
  | MATCH; e = expr; WITH; p = pattern_matching (* %prec MATCH *)
      { EMatch (e, p) }
  | TRY; e = expr; WITH; p = pattern_matching (* %prec MATCH *)
      { ETry (e, p) }
  | LET; b = llet; l = llet_ands; IN; e = expr %prec LET
      { ELet (b :: l, e) }
  | e1 = expr; COLONCOLON; e2 = expr
      { EConstr (Lident "Cons", [e1; e2])}
  | e1 = expr; CARET; e2 = expr
      { EApply (Lident "string_concat", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = expr; AT; e2 = expr
      { EApply (Lident "list_concat", [(e1, Nolabel); (e2, Nolabel)]) }
  | e1 = simple_expr; DOT; LPAREN; e2 = expr; RPAREN; LTMINUS; e3 = expr
      { EApply (Lident "array_set", [(e1, Nolabel); (e2, Nolabel); (e3, Nolabel)]) }
  | e1 = simple_expr; DOT; LBRACK; e2 = expr; RBRACK; LTMINUS; e3 = expr
      { EApply (Lident "string_set", [(e1, Nolabel); (e2, Nolabel); (e3, Nolabel)]) }

llet:
  | p = pattern; EQ; e = expr { (p, e) }

llet_ands:
  | { [] }
  | AND; b = llet; l = llet_ands { b :: l }

semi_separated_expr_list:
  | e = expr %prec list_prec { [e] }
  | l = semi_separated_expr_list; SEMICOLON; e = expr %prec list_prec { e :: l }

semi_separated_expr_list_opt:
  | l = semi_separated_expr_list; ioption(SEMICOLON) { List.rev l }

%inline pattern_line:
  p = pattern; MINUSGT; e = expr
     { (p, e) }

pattern_lines:
  | p = pattern_line { [p] }
  | p = pattern_line; BAR; l = pattern_lines { p :: l }

%inline pattern_matching:
  ioption(BAR); l = pattern_lines { l }

field_decl:
  | ioption(MUTABLE); f = LIDENT; COLON; type_ignore { f }

%inline type_representation:
  | ioption(BAR); l = separated_nonempty_list(BAR, constr_decl)
    { (ISum l) }
  | LBRACE; l = separated_semi_opt(field_decl); RBRACE
    { (IRecord l) }
  | type_ignore { IRebind }

constr_decl:
  | n = UIDENT { (n, false) }
  | n = UIDENT; OF; type_ignore { (n, true) }

labelled_args:
  | x = LIDENT { (x, Nolabel, None) }
  | TILDE; x = LIDENT { (x, Labelled x, None) }
  | QUESTION; x = LIDENT { (x, Optional x, None) }
  | LPAREN; x = LIDENT; COLON; type_ignore; RPAREN { (x, Nolabel, None) }
  | LPAREN; RPAREN { ("_", Nolabel, None) }
  | QUESTION; LPAREN; x = LIDENT; EQ; e = expr; RPAREN { (x, Optional x, Some e) }

letdef:
  | name = LIDENT; vars = list(labelled_args); EQ; body = expr { (name, vars, body) }

let_ands:
  | { [] }
  | AND; d = letdef; l = let_ands { d :: l }

typedef:
  | name = LIDENT; EQ; r = type_representation { (name, r) }

type_ands:
  | { [] }
  | AND; t = typedef; l = type_ands { t :: l }

definition:
  | LET; d = letdef; l = let_ands { MLet (false, d :: l) }
  | LET; REC; d = letdef; l = let_ands { MLet (true, d :: l) }
  | TYPE; t = typedef; l = type_ands { MTypedef (t :: l) }
  | EXCEPTION; e = constr_decl { MException (fst e, snd e) }
  | OPEN; m = longident_uident { MOpen m }
  | MODULE; n = UIDENT; EQ; STRUCT; l = list(semidefinition); END { MStruct (n, l) }
  | EXTERNAL; n = LIDENT; COLON; type_ignore; EQ; s = STRING { MExternal (n, s) }

semidefinition:
  | d = definition; ioption(SEMICOLONSEMICOLON) { d }

definitions:
  | l = list(semidefinition); EOF { l }
