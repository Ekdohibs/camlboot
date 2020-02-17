(use-modules (system base lalr) (srfi srfi-1))

(define ml-parser
  (lalr-parser
   (expect: 0)
   ;; Token definitions
   (LPAREN LBRACE RBRACE QUOTE TILDE
           QUESTION SEMICOLONSEMICOLON LBRACK RBRACK LBRACKBAR RBRACKBAR
           AND BEGIN END EXCEPTION EXTERNAL FUN IF IN MODULE
           MUTABLE OF OPEN REC STRUCT TRY TYPE WITH
           EOF STRING LIDENT UIDENT INT
           (right: MINUSGT)
           (left: BAR)
           (nonassoc: annot_prec)
           (nonassoc: LET MATCH)
           (right: SEMICOLON)
           (right: list_prec)
           (nonassoc: THEN)
           (nonassoc: ELSE)
           (nonassoc: LTMINUS)
           (right: COLONEQ)
           (nonassoc: comma_prec)
           (left: COMMA)
           (right: BARBAR)
           (right: AMPERAMPER)
           (left: EQ LTGT LT GT LTEQ GTEQ)
           (right: CARET AT)
           (right: COLONCOLON)
           (left: PLUS MINUS)
           (left: STAR)
           (nonassoc: label_prec)
           (nonassoc: COLON)
           (nonassoc: dot_prec)
           (nonassoc: DOT)
           (nonassoc: RPAREN)
           (nonassoc: BANG))
   ;; Rules
   (definitions
     (EOF) : #nil
     (semidefinition definitions) : (cons $1 $2))

   (list_semidefinition
     ( ) : #nil
     (semidefinition list_semidefinition) : (cons $1 $2))

   (semidefinition
    (definition) : $1
    (definition SEMICOLONSEMICOLON) : $1)

   (definition
     (LET letdef let_ands) : (list 'MLet #f (cons $2 $3))
     (LET REC letdef let_ands) : (list 'MLet #t (cons $3 $4))
     (TYPE typedef type_ands) : (list 'MTypedef (cons $2 $3))
     (EXCEPTION constr_decl) : (list 'MException (car $2) (cdr $2))
     (OPEN longident_uident) : (list 'MOpen $2)
     (MODULE UIDENT EQ STRUCT list_semidefinition END) : (list 'MStruct $2 $5)
     (EXTERNAL LIDENT COLON type_ignore EQ STRING) : (list 'MExternal $2 $6))

   (type_ands
    ( ) : #nil
    (AND typedef type_ands) : (cons $2 $3))

   (typedef
    (LIDENT EQ separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $3))
    (LIDENT EQ BAR separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $4))
    (LIDENT EQ LBRACE separated_semi_opt_field_decl RBRACE) : (cons $1 (list 'IRecord $4))
    (LIDENT EQ type_ignore) : (cons $1 (list 'IRebind)))

   (let_ands
    ( ) : #nil
    (AND letdef let_ands) : (cons $2 $3))

   (letdef
    (LIDENT list_labelled_args EQ expr) : (cons $1 (cons $2 $4)))

   (list_labelled_args
    ( ) : #nil
    (labelled_args list_labelled_args) : (cons $1 $2))

   (labelled_args
    (LIDENT) : (cons $1 (cons (list 'Nolabel) (list 'None)))
    (TILDE LIDENT) : (cons $2 (cons (list 'Labelled $2) (list 'None)))
    (QUESTION LIDENT) : (cons $2 (cons (list 'Optional $2) (list 'None)))
    (LPAREN LIDENT COLON type_ignore RPAREN) : (cons $2 (cons (list 'Nolabel) (list 'None)))
    (LPAREN RPAREN) : (cons "_" (cons (list 'Nolabel) (list 'None)))
    (QUESTION LPAREN LIDENT EQ expr RPAREN) : (cons $3 (cons (list 'Optional $3) (list 'Some $5))))

   (constr_decl
    (UIDENT) : (cons $1 0)
    (UIDENT OF type_count_stars) : (cons $1 (+ 1 $3)))

   (separated_nonempty_list_bar_constr_decl
    (constr_decl) : (cons $1 #nil)
    (constr_decl BAR separated_nonempty_list_bar_constr_decl) : (cons $1 $3))

   (separated_semi_opt_field_decl
    (field_decl) : (cons $1 #nil)
    (field_decl SEMICOLON) : (cons $1 #nil)
    (field_decl SEMICOLON separated_semi_opt_field_decl) : (cons $1 $3))

   (semi_separated_expr_list_opt
    (semi_separated_expr_list) : (reverse $1)
    (semi_separated_expr_list SEMICOLON) : (reverse $1))

   (semi_separated_expr_list
    (expr (prec: list_prec)) : (cons $1 #nil)
    (semi_separated_expr_list SEMICOLON expr (prec: list_prec)) : (cons $3 $1))

   (type_ignore
    ( ) : '()
    (STAR type_ignore) : '()
    (COMMA type_ignore) : '()
    (MINUSGT type_ignore) : '()
    (QUOTE type_ignore) : '()
    (longident_lident type_ignore) : '()
    (LPAREN type_ignore RPAREN type_ignore) : '())

   (type_count_stars
    ( ) : 0
    (STAR type_count_stars) : (+ 1 $2)
    (longident_lident type_count_stars) : $2
    (QUOTE type_count_stars) : $2
    (LPAREN type_ignore RPAREN type_count_stars) : $4)

   (constant
    (STRING) : (list 'CString $1)
    (LPAREN RPAREN) : (list 'CUnit)
    (BEGIN END) : (list 'CUnit)
    (INT) : (list 'CInt $1))

   (longident_uident
    (UIDENT) : (list 'Lident $1)
    (longident_uident DOT UIDENT) : (list 'Ldot $1 $3))

   (longident_constr
    (longident_uident (prec: dot_prec)) : $1)

   (longident_lident
    (LIDENT) : (list 'Lident $1)
    (longident_uident DOT LIDENT) : (list 'Ldot $1 $3))

   (option_semicolon
    ( ) : '()
    (SEMICOLON) : '())

   (record_list_expr
    (longident_lident EQ expr (prec: list_prec)) : (cons (cons $1 $3) #nil)
    (record_list_expr SEMICOLON longident_lident EQ expr (prec: list_prec)) : (cons (cons $3 $5) $1))

   (pattern_constr_args
    (LIDENT) : (cons $1 #nil)
    (LIDENT COMMA pattern_constr_args) : (cons $1 $3))

   (comma_separated_list2_lident
    (LIDENT COMMA LIDENT) : (cons $3 (cons $1 #nil))
    (comma_separated_list2_lident COMMA LIDENT) : (cons $3 $1))

   (comma_separated_list2_expr
    (expr COMMA expr) : (cons $3 (cons $1 #nil))
    (comma_separated_list2_expr COMMA expr) : (cons $3 $1))

   (pattern
    (LIDENT) : (list 'PVar $1)
    (longident_constr) : (list 'PConstructor $1 #nil)
    (longident_constr LIDENT) : (list 'PConstructor $1 (cons $2 #nil))
    (longident_constr LPAREN pattern_constr_args RPAREN) : (list 'PConstructor $1 $3)
    (comma_separated_list2_lident) : (list 'PConstructor (list 'Lident "") (reverse $1))
    (LBRACK RBRACK) : (list 'PConstructor (list 'Lident "Null") #nil)
    (LIDENT COLONCOLON LIDENT) : (list 'PConstructor (list 'Lident "Cons") (cons $1 (cons $3 #nil)))
    (LPAREN pattern COLON type_ignore RPAREN) : $2
    (LPAREN RPAREN) : (list 'PInt "0")
    (LPAREN pattern RPAREN) : $2
    (INT) : (list 'PInt $1))

   (simple_expr
    (longident_lident) : (list 'EVar $1)
    (constant) : (list 'EConstant $1)
    (longident_constr (prec: dot_prec)) : (list 'EConstr $1 #nil)
    (LPAREN expr RPAREN) : $2
    (BEGIN expr END) : $2
    (LPAREN expr COLON type_ignore RPAREN) : $2
    (simple_expr DOT longident_lident) : (list 'EGetfield $1 $3)
    (LBRACE record_list_expr option_semicolon RBRACE) : (list 'ERecord (reverse $2))
    (LBRACK RBRACK) : (list 'EConstr (list 'Lident "Null") #nil)
    (LBRACK semi_separated_expr_list_opt RBRACK) :
        (fold-right (lambda (e r) (list 'EConstr (list 'Lident "Cons") (list e r))) (list 'EConstr (list 'Lident "Null") #nil) $2)
    (LBRACKBAR RBRACKBAR) : (list 'EVar (list 'Ldot (list 'Lident "Array") "empty_array"))
    (BANG simple_expr) : (list 'EApply (list 'Lident "ref_get") (cons (cons $2 (list 'Nolabel)) #nil))
    (simple_expr DOT LPAREN expr RPAREN) :
        (list 'Eapply (list 'Lident "array_get")
              (cons (cons $1 (list 'Nolabel)) (cons (cons $4 (list 'Nolabel)) #nil)))
    (simple_expr DOT LBRACK expr RBRACK) :
        (list 'Eapply (list 'Lident "string_get")
              (cons (cons $1 (list 'Nolabel)) (cons (cons $4 (list 'Nolabel)) #nil))))

   (labelled_simple_expr
    (simple_expr) : (cons $1 (list 'Nolabel))
    (TILDE LIDENT (prec: label_prec)) : (cons (list 'EVar (list 'Lident $2)) (list 'Labelled $2))
    (QUESTION LIDENT (prec: label_prec)) : (cons (list 'EVar (list 'Lident $2)) (list 'Optional $2))
    (TILDE LIDENT COLON simple_expr) : (cons $4 (list 'Labelled $2))
    (QUESTION LIDENT COLON simple_expr) : (cons $4 (list 'Optional $2)))

   (nonempty_list_lident
    (LIDENT) : (cons $1 #nil)
    (LIDENT nonempty_list_lident) : (cons $1 $2))

   (nonempty_list_labelled_simple_expr
    (labelled_simple_expr) : (cons $1 #nil)
    (labelled_simple_expr nonempty_list_labelled_simple_expr) : (cons $1 $2))

   (expr
    (simple_expr) : $1
    (FUN nonempty_list_lident MINUSGT expr) : (list 'ELambda $2 $4)
    (longident_lident nonempty_list_labelled_simple_expr) : (list 'EApply $1 $2)
    (longident_constr simple_expr) : (list 'EConstr $1 (cons $2 #nil))
    (comma_separated_list2_expr (prec: comma_prec)) : (list 'EConstr (list 'Lident "") (reverse $1))
    (simple_expr DOT longident_lident LTMINUS expr) : (list 'ESetfield $1 $3 $5)
    (IF expr THEN expr ELSE expr) : (list 'EIf $1 $3 $5)
    (IF expr THEN expr) : (list 'EIf $1 $3 (list 'EConstant (list 'CUnit)))
    (expr SEMICOLON expr) : (list 'EChain $1 $3)
    (expr EQ expr) :
      (list 'Eapply (list 'Lident "eq")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr LTGT expr) :
      (list 'Eapply (list 'Lident "neq")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr LT expr) :
      (list 'Eapply (list 'Lident "lessthan")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr GT expr) :
      (list 'Eapply (list 'Lident "lessthan")
            (cons (cons $3 (list 'Nolabel)) (cons (cons $1 (list 'Nolabel)) #nil)))
    (expr LTEQ expr) :
      (list 'Eapply (list 'Lident "lessequal")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr GTEQ expr) :
      (list 'Eapply (list 'Lident "lessequal")
            (cons (cons $3 (list 'Nolabel)) (cons (cons $1 (list 'Nolabel)) #nil)))
    (expr PLUS expr) :
      (list 'Eapply (list 'Lident "plus")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr MINUS expr) :
      (list 'Eapply (list 'Lident "minus")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr STAR expr) :
      (list 'Eapply (list 'Lident "times")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr COLONEQ expr) :
      (list 'Eapply (list 'Lident "ref_set")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr AMPERAMPER expr) : (list 'EIf $1 $3 (list 'EConstant (list 'CInt 0)))
    (expr BARBAR expr) : (list 'EIf $1 (list 'EConstant (list 'CInt 1)) $3)
    (MATCH expr WITH pattern_lines) : (list 'EMatch $2 $4)
    (TRY expr WITH pattern_lines) : (list 'ETry $2 $4)
    (MATCH expr WITH BAR pattern_lines) : (list 'EMatch $2 $5)
    (TRY expr WITH BAR pattern_lines) : (list 'ETry $2 $5)
    (LET llet llet_ands IN expr (prec: LET)) : (list 'ELet (cons $2 $3) $5)
    (expr COLONCOLON expr) : (list 'EConstr (list 'Lident "Cons") (cons $1 (cons $3 #nil)))
    (expr CARET expr) :
      (list 'Eapply (list 'Lident "string_concat")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (expr AT expr) :
      (list 'Eapply (list 'Lident "list_concat")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $3 (list 'Nolabel)) #nil)))
    (simple_expr DOT LPAREN expr RPAREN LTMINUS expr) :
      (list 'Eapply (list 'Lident "array_set")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $4 (list 'Nolabel)) (cons (cons $7 (list 'Nolabel)) #nil))))
    (simple_expr DOT LBRACK expr RBRACK LTMINUS expr) :
      (list 'Eapply (list 'Lident "string_set")
            (cons (cons $1 (list 'Nolabel)) (cons (cons $4 (list 'Nolabel)) (cons (cons $7 (list 'Nolabel)) #nil))))
    )

   (llet
    (pattern EQ expr) : (cons $1 $3))

   (llet_ands
    ( ) : #nil
    (AND llet llet_ands) : (cons $2 $3))

   (pattern_lines
    (pattern MINUSGT expr) : (cons (cons $1 $3) #nil)
    (pattern MINUSGT expr BAR pattern_lines) : (cons (cons $1 $3) $5))

   (field_decl
    (LIDENT COLON type_ignore) : $1
    (MUTABLE LIDENT COLON type_ignore) : $2)


   ))
