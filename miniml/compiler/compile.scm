(use-modules (system base lalr)
             (srfi srfi-1) (srfi srfi-9 gnu)
             (rnrs base)
             (ice-9 q)
             (ice-9 binary-ports) (ice-9 vlist) (ice-9 match))


(define error-code-syntax 3)

(define-immutable-record-type <def>
  (mkdef_ name args body)
  def?
  (name def-get-name)
  (args def-get-args)
  (body def-get-body))

(define-immutable-record-type <arg>
  (mkarg pat label default)
  arg?
  (pat arg-get-pat)
  (label arg-get-label)
  (default arg-get-default))

(define (mknolabelapp arg) (cons arg (list 'Nolabel)))
(define (mknolabelfun arg) (mkarg arg (list 'Nolabel) (list 'None)))

(define (lid->evar v) (list 'EVar (list 'Lident v)))
(define (lid->lvar v) (list 'LVar v))
(define (lid->pvar v) (list 'PVar v))
(define (lid->econstr v args) (list 'EConstr (list 'Lident v) args))
(define (lid->pconstr v args) (list 'PConstr (list 'Lident v) args))

(define (mkapp fname args) (list 'EApply (list 'Lident fname) (map mknolabelapp args)))
(define (mkapp1 fname arg1) (mkapp fname (list arg1)))
(define (mkapp2 fname arg1 arg2) (mkapp fname (list arg1 arg2)))
(define (mkapp3 fname arg1 arg2 arg3) (mkapp fname (list arg1 arg2 arg3)))

(define (mklambda args body)
  (match body
         (('ELambda args2 body2) (list 'ELambda (append args args2) body2))
         (_ (list 'ELambda args body))
  ))

(define (mkdef name args body)
  (match body
         (('ELambda args2 body2) (mkdef_ name (append args args2) body2))
         (_ (mkdef_ name args body))
  ))

(define (mkfunctor args body)
  (fold-right (lambda (arg b) (list 'MEFunctor arg b)) body args))

; note: ml-parser is a procedure rather than a variable, because we were not able
; to call the same lalr-parser on several input files in a reliable way.
; (There is no documentation about this, or in general very little
; documentation about lalr-parser.) The bug we would observe
; is that calling (ml-parser lexer errorp) a second time after changing
; the default input port would return an empty document. There seems
; to be some per-parser global state that we don't know how to (re)initialize.
(define (ml-parser)
  (lalr-parser
   (expect: 0)
   ;; Token definitions
   (LPAREN LBRACE RBRACE QUOTE TILDE
           QUESTION SEMICOLONSEMICOLON LBRACK RBRACK LBRACKBAR BARRBRACK
           AND BEGIN END EXCEPTION EXTERNAL FUN FUNCTION FUNCTOR IF IN MODULE
           MUTABLE OF OPEN REC SIG STRUCT TRY TYPE VAL WITH
           EOF STRING LIDENT UIDENT INT
           (right: MINUSGT)
           (left: BAR)
           (left: AS)
           (nonassoc: annot_prec)
           (nonassoc: LET MATCH)
           (right: SEMICOLON)
           (nonassoc: THEN)
           (nonassoc: ELSE)
           (nonassoc: LTMINUS)
           (right: COLONEQ)
           (nonassoc: comma_prec)
           (left: COMMA)
           (right: BARBAR)
           (right: AMPERAMPER)
           (left: EQ BARGT INFIXOP0)
           (right: ATAT INFIXOP1)
           (right: COLONCOLON)
           (left: PLUS MINUS INFIXOP2)
           (left: STAR PERCENT INFIXOP3)
           (right: INFIXOP4)
           (nonassoc: uminus_prec)
           (nonassoc: label_prec)
           (nonassoc: COLON)
           (nonassoc: dot_prec)
           (nonassoc: DOT)
           (nonassoc: RPAREN)
           (nonassoc: PREFIXOP))
   ;; Rules
   (definitions
     ( ) : #nil
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
     (MODULE UIDENT functor_args EQ module_expr) : (list 'MModule $2 (mkfunctor $3 $5))
     (MODULE TYPE UIDENT EQ module_type) : (list 'MModuleType $3 $5)
     (EXTERNAL lident_ext COLON type_count_arrows EQ STRING) : (list 'MExternal $2 $4 $6))

   (module_expr
    (STRUCT list_semidefinition END) : (list 'MEStruct $2)
    (FUNCTOR functor_args MINUSGT module_expr) : (mkfunctor $2 $4)
    (longident_uident functor_apply) : (list 'MEApply $1 $2))

   (functor_apply
    ( ) : #nil
    (LPAREN module_expr RPAREN functor_apply) : (cons $2 $4))

   (signature_item
    (TYPE typedef type_ands) : '()
    (VAL lident_ext COLON type_ignore) : '())

   (signature
    ( ) : '()
    (SEMICOLONSEMICOLON signature) : '()
    (signature_item signature) : '())

   (module_type
    (longident_uident) : '()
    (SIG signature END) : '())

   (functor_args
    ( ) : #nil
    (functor_arg functor_args) : (cons $1 $2))

   (functor_arg
    (LPAREN UIDENT COLON module_type RPAREN) : $2)

   (type_ands
    ( ) : #nil
    (AND typedef type_ands) : (cons $2 $3))

   (type_name_with_args
    (LIDENT) : $1
    (variance QUOTE LIDENT LIDENT) : $4
    (LPAREN type_ignore RPAREN LIDENT) : $4)

   (variance
    (nonempty_variance) : '()
    ( ) : '()
   )

   (nonempty_variance
    (PLUS) : '()
    (MINUS) : '()
   )

   (typedef
    (type_name_with_args) : (cons $1 (list 'IRebind))
    (type_name_with_args EQ separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $3))
    (type_name_with_args EQ BAR separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $4))
    (type_name_with_args EQ record_def) : (cons $1 (list 'IRecord $3))
    (type_name_with_args EQ type_ignore) : (cons $1 (list 'IRebind)))

   (record_def
    (LBRACE separated_semi_opt_field_decl RBRACE) : $2)

   (let_ands
    ( ) : #nil
    (AND letdef let_ands) : (cons $2 $3))

   (letdef
    (LPAREN RPAREN EQ expr) : (mkdef "_" #nil $4)
    (lident_ext list_labelled_arg EQ expr) : (mkdef $1 $2 $4))

   (list_labelled_arg
    ( ) : #nil
    (nonempty_list_labelled_arg) : $1)

   (nonempty_list_labelled_arg
    (labelled_arg) : (list $1)
    (labelled_arg nonempty_list_labelled_arg) : (cons $1 $2))

   (labelled_arg
    (simple_pattern) : (mknolabelfun $1)
    (TILDE LIDENT) : (mkarg (list 'PVar $2) (list 'Labelled $2) (list 'None))
    (QUESTION LIDENT) : (mkarg (list 'PVar $2) (list 'Optional $2) (list 'None))
    (QUESTION LPAREN LIDENT EQ expr RPAREN) : (mkarg (list 'PVar $3) (list 'Optional $3) (list 'Some $5)))

   (constr_decl
    (uident_ext) : (cons $1 (list 'VariantTuple 0))
    (uident_ext OF type_count_stars) : (cons $1 (list 'VariantTuple (+ 1 $3)))
    (uident_ext OF record_def) : (cons $1 (list 'VariantRecord $3))
   )

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
    (expr_no_semi) : (cons $1 #nil)
    (semi_separated_expr_list SEMICOLON expr_no_semi) : (cons $3 $1))

   (semi_separated_pattern_list_opt
    (semi_separated_pattern_list) : (reverse $1)
    (semi_separated_pattern_list SEMICOLON) : (reverse $1))

   (semi_separated_pattern_list
    (pattern) : (cons $1 #nil)
    (semi_separated_pattern_list SEMICOLON pattern) : (cons $3 $1))

   (type_ignore
    ( ) : '()
    (STAR type_ignore) : '()
    (COMMA type_ignore) : '()
    (MINUSGT type_ignore) : '()
    (nonempty_variance type_ignore) : '()
    (QUOTE type_ignore) : '()
    (longident_field type_ignore) : '()
    (LPAREN type_ignore RPAREN type_ignore) : '())

   (type_count_stars
    ( ) : 0
    (STAR type_count_stars) : (+ 1 $2)
    (longident_field type_count_stars) : $2
    (QUOTE type_count_stars) : $2
    (LPAREN type_ignore RPAREN type_count_stars) : $4)

   (type_count_arrows
    ( ) : 0
    (MINUSGT type_count_arrows) : (+ 1 $2)
    (longident_field type_count_arrows) : $2
    (QUOTE type_count_arrows) : $2
    (STAR type_count_arrows) : $2
    (LPAREN type_ignore RPAREN type_count_arrows) : $4)

   (constant
    (STRING) : (list 'CString $1)
    (LPAREN RPAREN) : (list 'CUnit)
    (BEGIN END) : (list 'CUnit)
    (INT) : (list 'CInt $1))

   (uident_symb
    (LBRACK RBRACK) : "[]"
    (LPAREN COLONCOLON RPAREN) : "::")

   (uident_ext
    (UIDENT) : $1
    (uident_symb) : $1)

   (longident_uident
    (UIDENT) : (list 'Lident $1)
    (longident_uident DOT UIDENT) : (list 'Ldot $1 $3))

   (longident_uident_ext
    (longident_uident) : $1
    (longident_uident DOT uident_symb) : (list 'Ldot $1 $3))

   (longident_constr
    (longident_uident_ext (prec: dot_prec)) : $1)

   (lident_symb
    (COLONEQ) : ":="
    (EQ) : "="
    (PLUS) : "+"
    (MINUS) : "-"
    (STAR) : "*"
    (PERCENT) : "%"
    (INFIXOP0) : $1
    (INFIXOP1) : $1
    (INFIXOP2) : $1
    (INFIXOP3) : $1
    (INFIXOP4) : $1
    (PREFIXOP) : $1)

   (lident_ext
    (LIDENT) : $1
    (LPAREN lident_symb RPAREN) : $2)

   (longident_lident
    (lident_ext) : (list 'Lident $1)
    (longident_uident DOT lident_ext) : (list 'Ldot $1 $3))

   (longident_field
    (LIDENT) : (list 'Lident $1)
    (longident_uident DOT LIDENT) : (list 'Ldot $1 $3))

   (option_semicolon
    ( ) : '()
    (SEMICOLON) : '())

   (record_list_expr
    (record_item_expr) : (cons $1 #nil)
    (record_list_expr SEMICOLON record_item_expr) : (cons $3 $1))

   (record_item_expr
    (longident_field EQ expr_no_semi) : (cons $1 $3)
    (LIDENT) : (cons (list 'Lident $1) (lid->evar $1))
   )

   (record_list_pattern
    (record_item_pattern) : (cons $1 #nil)
    (record_list_pattern SEMICOLON record_item_pattern) : (cons $3 $1))

   (record_item_pattern
    (longident_field EQ pattern) : (cons $1 $3)
    (LIDENT) : (cons (list 'Lident $1) (lid->pvar $1))
   )

   (comma_separated_list2_pattern
    (pattern COMMA pattern) : (cons $3 (cons $1 #nil))
    (comma_separated_list2_pattern COMMA pattern) : (cons $3 $1))

   (comma_separated_list2_expr
    (expr_no_semi COMMA expr_no_semi) : (cons $3 (cons $1 #nil))
    (comma_separated_list2_expr COMMA expr_no_semi) : (cons $3 $1))

   (pattern
    (simple_pattern) : $1
    (longident_constr simple_pattern) : (list 'PConstr $1 (cons $2 #nil))
    (comma_separated_list2_pattern (prec: comma_prec)) : (lid->pconstr "" (reverse $1))
    (pattern BAR pattern) : (list 'POr $1 $3)
    (pattern COLONCOLON pattern) : (lid->pconstr "::" (cons $1 (cons $3 #nil)))
    (pattern AS LIDENT) : (list 'PAs $1 $3)
   )

   (simple_pattern
    (lident_ext) : (if (equal? $1 "_") (list 'PWild) (list 'PVar $1))
    (longident_constr) : (list 'PConstr $1 #nil)
    (LBRACK RBRACK) : (lid->pconstr "[]" #nil)
    (LBRACK semi_separated_pattern_list_opt RBRACK) :
        (fold-right (lambda (p r) (lid->pconstr "::" (list p r))) (lid->pconstr "[]" #nil) $2)
    (LPAREN pattern COLON type_ignore RPAREN) : $2
    (LPAREN RPAREN) : (list 'PInt 0)
    (LPAREN pattern RPAREN) : $2
    (LBRACE record_list_pattern option_semicolon RBRACE) : (list 'PRecord (reverse $2))
    (INT) : (list 'PInt $1))

   (simple_expr
    (longident_lident) : (list 'EVar $1)
    (constant) : (list 'EConstant $1)
    (longident_constr (prec: dot_prec)) : (list 'EConstr $1 #nil)
    (LPAREN expr RPAREN) : $2
    (BEGIN expr END) : $2
    (LPAREN expr COLON type_ignore RPAREN) : $2
    (simple_expr DOT longident_field) : (list 'EGetfield $1 $3)
    (LBRACE record_list_expr option_semicolon RBRACE) : (list 'ERecord (reverse $2))
    (LBRACE simple_expr WITH record_list_expr option_semicolon RBRACE) : (list 'ERecordwith $2 (reverse $4))
    (LBRACK RBRACK) : (lid->econstr "[]" #nil)
    (LBRACK semi_separated_expr_list_opt RBRACK) :
        (fold-right (lambda (e r) (lid->econstr "::" (list e r))) (lid->econstr "[]" #nil) $2)
    (LBRACKBAR BARRBRACK) : (list 'EVar (list 'Ldot (list 'Lident "Array") "empty_array"))
    (LBRACKBAR semi_separated_expr_list_opt BARRBRACK) : (lid->econstr "" $2)
    (PREFIXOP simple_expr) : (mkapp1 $1 $2)
    (simple_expr DOT LPAREN expr RPAREN) : (mkapp2 "array_get" $1 $4)
    (simple_expr DOT LBRACK expr RBRACK) : (mkapp2 "string_get" $1 $4))

   (labelled_simple_expr
    (simple_expr) : (mknolabelapp $1)
    (TILDE LIDENT (prec: label_prec)) : (cons (list 'EVar (list 'Lident $2)) (list 'Labelled $2))
    (QUESTION LIDENT (prec: label_prec)) : (cons (list 'EVar (list 'Lident $2)) (list 'Optional $2))
    (TILDE LIDENT COLON simple_expr) : (cons $4 (list 'Labelled $2))
    (QUESTION LIDENT COLON simple_expr) : (cons $4 (list 'Optional $2)))

   (nonempty_list_lident
    (lident_ext) : (cons $1 #nil)
    (lident_ext nonempty_list_lident) : (cons $1 $2))

   (list_labelled_simple_expr
    ( ) : #nil
    (nonempty_list_labelled_simple_expr) : $1)

   (nonempty_list_labelled_simple_expr
    (labelled_simple_expr) : (cons $1 #nil)
    (labelled_simple_expr nonempty_list_labelled_simple_expr) : (cons $1 $2))

   (expr_no_semi
    (simple_expr) : $1
    (FUN nonempty_list_labelled_arg MINUSGT expr) : (mklambda $2 $4)
    (longident_lident nonempty_list_labelled_simple_expr) : (list 'EApply $1 $2)
    (longident_constr simple_expr) : (list 'EConstr $1 (cons $2 #nil))
    (comma_separated_list2_expr (prec: comma_prec)) : (list 'EConstr (list 'Lident "") (reverse $1))
    (simple_expr DOT longident_field LTMINUS expr_no_semi) : (list 'ESetfield $1 $3 $5)
    (IF expr THEN expr ELSE expr) : (list 'EIf $2 $4 $6)
    (IF expr THEN expr) : (list 'EIf $2 $4 (list 'EConstant (list 'CUnit)))
    (expr_no_semi INFIXOP0 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi INFIXOP1 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi INFIXOP2 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi INFIXOP3 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi INFIXOP4 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi EQ expr_no_semi) : (mkapp2 "=" $1 $3)
    (expr_no_semi PLUS expr_no_semi) : (mkapp2 "+" $1 $3)
    (expr_no_semi MINUS expr_no_semi) : (mkapp2 "-" $1 $3)
    (MINUS expr_no_semi (prec: uminus_prec)) : (mkapp1 "~-" $2)
    (expr_no_semi PERCENT expr_no_semi) : (mkapp2 "%" $1 $3)
    (expr_no_semi STAR expr_no_semi) : (mkapp2 "*" $1 $3)
    (expr_no_semi COLONEQ expr_no_semi) : (mkapp2 ":=" $1 $3)
    (expr_no_semi AMPERAMPER expr_no_semi) : (list 'EIf $1 $3 (list 'EConstant (list 'CInt 0)))
    (expr_no_semi BARBAR expr_no_semi) : (list 'EIf $1 (list 'EConstant (list 'CInt 1)) $3)
    (expr_no_semi BARGT longident_lident list_labelled_simple_expr):
      ;; (e |> f e1 e2 .. en) ~> f e1 .. en e
      (list 'EApply $3 (append $4 (list (mknolabelapp $1))))
    (longident_lident list_labelled_simple_expr ATAT expr_no_semi):
      ;; (f e1 .. en @@ e) ~> f e1 .. en e
      (list 'EApply $1 (append $2 (list (mknolabelapp $4))))
    (MATCH expr WITH clauses) : (list 'EMatch $2 $4)
    (TRY expr WITH clauses) : (list 'ETry $2 $4)
    (FUNCTION clauses) :
      (mklambda
       (list (mknolabelfun (lid->pvar "arg#function")))
       (list 'EMatch (lid->evar "arg#function") $2))
    (LET llet llet_ands IN expr (prec: LET)) : (list 'ELet #f (cons $2 $3) $5)
    (LET REC llet llet_ands IN expr (prec: LET)) : (list 'ELet #t (cons $3 $4) $6)
    (LET OPEN longident_uident IN expr (prec: LET)) : (list 'ELetOpen $3 $5)
    (longident_uident DOT LPAREN expr RPAREN) : (list 'ELetOpen $1 $4)
    (expr_no_semi COLONCOLON expr_no_semi) : (lid->econstr "::" (cons $1 (cons $3 #nil)))
    (simple_expr DOT LPAREN expr RPAREN LTMINUS expr_no_semi) : (mkapp3 "array_set" $1 $4 $7)
    (simple_expr DOT LBRACK expr RBRACK LTMINUS expr_no_semi) : (mkapp3 "string_set" $1 $4 $7)
    (LET percent_exit llet llet_ands IN expr (prec: LET)) : (list 'ELetExits (cons $3 $4) $6)
    (LBRACK percent_exit RBRACK LIDENT list_labelled_simple_expr) : (list 'EExit $4 $5)
    )

   (percent_exit
    (PERCENT LIDENT) : (if (equal? $2 "exit") #nil
                           (errorp "expected the %exit extension")))

   (expr
    (expr_no_semi) : $1
    (expr SEMICOLON expr) : (list 'EChain $1 $3))

   (llet
    (pattern EQ expr) : (cons $1 $3)
    (lident_ext nonempty_list_labelled_arg EQ expr) : (cons (list 'PVar $1) (mklambda $2 $4)))

   (llet_ands
    ( ) : #nil
    (AND llet llet_ands) : (cons $2 $3))

   (clauses
    (clauses_without_bar) : $1
    (BAR clauses_without_bar) : $2)

   (clauses_without_bar
    (clause) : (list $1)
    (clause BAR clauses_without_bar) : (cons $1 $3))

   (clause
    (pattern MINUSGT expr) : (cons $1 $3))

   (field_decl
    (LIDENT COLON type_ignore) : $1
    (MUTABLE LIDENT COLON type_ignore) : $2)


 ))

(define kw (list
    (cons "and" (cons 'AND #f))
    (cons "as" (cons 'AS #f))
    (cons "asr" (cons 'INFIXOP4 "asr"))
    (cons "begin" (cons 'BEGIN #f))
    (cons "else" (cons 'ELSE #f))
    (cons "end" (cons 'END #f))
    (cons "exception" (cons 'EXCEPTION #f))
    (cons "external" (cons 'EXTERNAL #f))
    (cons "false" (cons 'UIDENT "false"))
    (cons "fun" (cons 'FUN #f))
    (cons "function" (cons 'FUNCTION #f))
    (cons "functor" (cons 'FUNCTOR #f))
    (cons "if" (cons 'IF #f))
    (cons "in" (cons 'IN #f))
    (cons "let" (cons 'LET #f))
    (cons "land" (cons 'INFIXOP3 "land"))
    (cons "lor" (cons 'INFIXOP3 "lor"))
    (cons "lxor" (cons 'INFIXOP3 "lxor"))
    (cons "lsl" (cons 'INFIXOP4 "lsl"))
    (cons "lsr" (cons 'INFIXOP4 "lsr"))
    (cons "match" (cons 'MATCH #f))
    (cons "mod" (cons 'INFIXOP3 "mod"))
    (cons "module" (cons 'MODULE #f))
    (cons "mutable" (cons 'MUTABLE #f))
    (cons "of" (cons 'OF #f))
    (cons "open" (cons 'OPEN #f))
    (cons "rec" (cons 'REC #f))
    (cons "sig" (cons 'SIG #f))
    (cons "struct" (cons 'STRUCT #f))
    (cons "then" (cons 'THEN #f))
    (cons "true" (cons 'UIDENT "true"))
    (cons "try" (cons 'TRY #f))
    (cons "type" (cons 'TYPE #f))
    (cons "val" (cons 'VAL #f))
    (cons "with" (cons 'WITH #f))
    ))

(define operator-kw (list
   (cons "&&" (cons 'AMPERAMPER #f))
   (cons "@@" (cons 'ATAT #f))
   (cons "!=" (cons 'INFIXOP0 "!="))
   (cons "|" (cons 'BAR #f))
   (cons "||" (cons 'BARBAR #f))
   (cons "|>" (cons 'BARGT #f))
   (cons "=" (cons 'EQ #f))
   (cons "+" (cons 'PLUS #f))
   (cons "<-" (cons 'LTMINUS #f))
   (cons "-" (cons 'MINUS #f))
   (cons "->" (cons 'MINUSGT #f))
   (cons "?" (cons 'QUESTION #f))
   (cons "*" (cons 'STAR #f))
   (cons "~" (cons 'TILDE #f))
   (cons "%" (cons 'PERCENT #f))
   ))


(define (get-lident s)
  (let ((p (assoc s kw)))
    (if (pair? p) (cdr p) (cons 'LIDENT s))))

(define (mktoken location tk) (make-lexical-token (car tk) location (cdr tk)))

(define (current-location)
  (make-source-location "*stdin*" (port-line (current-input-port)) (port-column (current-input-port)) -1 -1))

(define (comment errorp)
  (let* ((location (current-location))
         (c (read-char)))
    (cond ((eof-object? c) (errorp "Unterminated comment"))
          ((char=? c #\*) (if (char=? (peek-char) #\)) (begin (read-char) #f) (comment errorp)))
          ((char=? c #\() (if (char=? (peek-char) #\*) (begin (read-char) (comment errorp) (comment errorp)) (comment errorp)))
          (else (comment errorp))
  )))

(define (char-to-hex c)
  (cond ((char-numeric? c) (- (char->integer c) (char->integer #\0)))
        ((and (char>=? c #\a) (char<=? c #\f)) (+ 10 (- (char->integer c) (char->integer #\a))))
        ((and (char>=? c #\A) (char<=? c #\F)) (+ 10 (- (char->integer c) (char->integer #\A))))))

(define (escape-sequence errorp)
  (let* ((location (current-location))
         (c (read-char)))
    (cond ((eof-object? c) (errorp "Unterminated escape sequence"))
          ((char=? c #\\ ) #\\ )
          ((char=? c #\" ) #\" )
          ((char=? c #\' ) #\' )
          ((char=? c #\n ) #\newline)
          ((char=? c #\r ) #\cr)
          ((char=? c #\t ) #\tab)
          ((char=? c #\b ) #\bs)
          ((char=? c #\space) #\space)
          ((char=? c #\x)
           (let* ((c2 (read-char))
                  (c3 (read-char)))
             (integer->char (+ (* 16 (char-to-hex c2)) (char-to-hex c3)))
             ))
          ((char=? c #\o)
           (let* ((c2 (read-char))
                  (c3 (read-char))
                  (c4 (read-char)))
             (integer->char (+ (* 64 (char-to-hex c2)) (+ (* 8 (char-to-hex c3)) (char-to-hex c4))))
             ))
          ((char-numeric? c)
           (let* ((c2 (read-char))
                  (c3 (read-char)))
             (assert (char-numeric? c2))
             (assert (char-numeric? c3))
             (integer->char (+ (* 100 (char-to-hex c)) (+ (* 10 (char-to-hex c2)) (char-to-hex c3))))
             ))
          (else (errorp "Invalid escape sequence" c)))
  ))

(define (space-or-tab? c) (or (char=? c #\space) (char=? c #\tab)))

(define (string-chars errorp)
  (let* ((location (current-location))
         (c (read-char)))
    (cond ((eof-object? c) (errorp "Unterminated string"))
          ((char=? c #\") #nil)
          ((char=? c #\\ )
           (if (char=? (peek-char) #\newline)
               (begin (read-char) (while (space-or-tab? (peek-char)) (read-char)) (string-chars errorp))
               (let* ((nc (escape-sequence errorp))) (cons nc (string-chars errorp)))))
          (else (cons c (string-chars errorp)))
  )))

(define (char-alphanumeric? c) (or (char-alphabetic? c) (char-numeric? c)))

(define (ident errorp)
  (let ((c (peek-char)))
        (cond ((eof-object? c) #nil)
              ((or (char-alphanumeric? c) (or (char=? c #\_) (char=? c #\'))) (begin (read-char) (cons c (ident errorp))))
              (else #nil)
              )))

(define (number-chars errorp)
  (let ((c (peek-char)))
    (cond ((eof-object? c) #nil)
          ((char-numeric? c) (begin (read-char) (cons c (number-chars errorp))))
          (else #nil)
          )))

(define (symbol-char? c)
  (string-index "!$%&*+-./:<=>?@^|~" c))

(define (symbol-chars)
  (let ((c (peek-char)))
    (cond ((eof-object? c) #nil)
          ((symbol-char? c) (begin (read-char) (cons c (symbol-chars))))
          (else #nil)
          )))

(define (get-operator name)
  (let ((p (assoc name operator-kw))
        (c (string-ref name 0)))
    (cond ((pair? p) (cdr p))
          ((string-index "!~?" c) (cons 'PREFIXOP name))
          ((string-index "=<>|&$" c) (cons 'INFIXOP0 name))
          ((string-index "@^" c) (cons 'INFIXOP1 name))
          ((string-index "+-" c) (cons 'INFIXOP2 name))
          ((and (char=? c #\*) (> (string-length name) 1) (char=? (string-ref name 1) #\*)) (cons 'INFIXOP4 name))
          ((string-index "*/%" c) (cons 'INFIXOP3 name)))))

(define (mksymbol location c)
  (mktoken location (get-operator (list->string (cons c (symbol-chars))))))

(define (skip-until-newline)
  (let ((c (read-char)))
    (cond ((eof-object? c) '())
          ((char=? c #\newline) '())
          (else (skip-until-newline))
    )))

(define (token errorp)
  (let* ((location (current-location))
         (c (read-char)))
    (token-dispatch errorp location c)))

(define (token-dispatch errorp location c)
  (cond ((eof-object? c) (make-lexical-token '*eoi* location #f))
        ((space-or-tab? c) (token errorp))
        ((char=? c #\newline) (token errorp))
        ((char=? c #\#) (if (= (port-column (current-input-port)) 1)
                            (begin (skip-until-newline) (token errorp))
                            (errorp "Illegal character: " c)))
        ((char=? c #\)) (make-lexical-token 'RPAREN location #f))
        ((char=? c #\() (if (char=? (peek-char) #\*)
                            (begin (read-char) (comment errorp) (token errorp))
                            (make-lexical-token 'LPAREN location #f)))
        ((char=? c #\{) (make-lexical-token 'LBRACE location #f))
        ((char=? c #\}) (make-lexical-token 'RBRACE location #f))
        ((char=? c #\,) (make-lexical-token 'COMMA location #f))
        ((char=? c #\[) (if (char=? (peek-char) #\|)
                            (begin (read-char) (make-lexical-token 'LBRACKBAR location #f))
                            (make-lexical-token 'LBRACK location #f)))
        ((char=? c #\]) (make-lexical-token 'RBRACK location #f))
        ((char=? c #\;) (if (char=? (peek-char) #\;)
                            (begin (read-char) (make-lexical-token 'SEMICOLONSEMICOLON location #f))
                            (make-lexical-token 'SEMICOLON location #f)))
        ((char=? c #\.) (make-lexical-token 'DOT location #f))
        ((char=? c #\:) (if (char=? (peek-char) #\:)
                            (begin (read-char) (make-lexical-token 'COLONCOLON location #f))
                            (if (char=? (peek-char) #\=)
                                (begin (read-char) (make-lexical-token 'COLONEQ location #f))
                                (make-lexical-token 'COLON location #f))))
        ; Handle '|' separately because of the "|]" token
        ((char=? c #\|) (cond
                         ((char=? (peek-char) #\])
                          (read-char) (make-lexical-token 'BARRBRACK location #f))
                         (else (mksymbol location c))))
        ; Handle '-' separately because of negative integer literals
        ((char=? c #\-) (if (char-numeric? (peek-char))
                            (make-lexical-token 'INT location
                                                (- (string->number (list->string (number-chars errorp)))))
                            (mksymbol location c)))
        ; All other characters that can begin an operator
        ((string-index "+=*~@^?!&<>/%$" c) (mksymbol location c))
        ((char=? c #\") (make-lexical-token 'STRING location (list->string (string-chars errorp))))
        ((char=? c #\') (let ((c (read-char)))
                             (if (char=? c #\\ )
                                 (let* ((nc (escape-sequence errorp))
                                        (c2 (read-char)))
                                   (if (char=? c2 #\')
                                       (make-lexical-token 'INT location (char->integer nc))
                                       (errorp "Unterminated character literal")
                                   ))
                                 (if (char=? (peek-char) #\')
                                     (begin
                                       (read-char) (make-lexical-token 'INT location (char->integer c)))
                                     (begin (unread-char c) (make-lexical-token 'QUOTE location #f))))
                             ))
        ((or (char-lower-case? c) (char=? c #\_)) (mktoken location (get-lident (list->string (cons c (ident errorp))))))
        ((char-upper-case? c) (make-lexical-token 'UIDENT location (list->string (cons c (ident errorp)))))
        ((char-numeric? c) (make-lexical-token 'INT location (string->number (list->string (cons c (number-chars errorp))))))
        (else (errorp "Illegal character: " c))
        ))

(define errorp (lambda (message . args)
  (begin
    (display message)
    (if (and (pair? args)
             (lexical-token? (car args)))
        (let ((token (car args)))
          (display (lexical-token-category token))
          (display " ")
          (display (lexical-token-value token))
          (let ((source (lexical-token-source token)))
            (if (source-location? source)
                (let ((line (source-location-line source))
                      (column (source-location-column source)))
                  (if (and (number? line) (number? column))
                      (begin
                        (display " (at line ")
                        (display line)
                        (display ", column ")
                        (display (+ 1 column))
                        (display ")")))))))
        (for-each display args))
    (newline)
    (backtrace)
    (exit error-code-syntax))))

(define bytecode-output-port #nil)
(define bytecode-sections #nil)
(define bytecode-current-section #nil)
(define (bytecode-open-output file)
  (begin
    (set! bytecode-output-port (open-output-file file))
    (set! bytecode-sections #nil)
    (set! bytecode-current-section #nil)
    ))

(define (put-u16 port c)
  (begin
    (put-u8 port (logand #xff (ash c -8)))
    (put-u8 port (logand #xff c))
    ))
(define (put-u32 port c)
  (begin
    (put-u8 port (logand #xff (ash c -24)))
    (put-u8 port (logand #xff (ash c -16)))
    (put-u8 port (logand #xff (ash c -8)))
    (put-u8 port (logand #xff c))
    ))
(define (put-u64 port c)
  (begin
    (put-u8 port (logand #xff (ash c -56)))
    (put-u8 port (logand #xff (ash c -48)))
    (put-u8 port (logand #xff (ash c -40)))
    (put-u8 port (logand #xff (ash c -32)))
    (put-u8 port (logand #xff (ash c -24)))
    (put-u8 port (logand #xff (ash c -16)))
    (put-u8 port (logand #xff (ash c -8)))
    (put-u8 port (logand #xff c))
    ))
(define (put-u16-le port c)
  (begin
    (put-u8 port (logand #xff c))
    (put-u8 port (logand #xff (ash c -8)))
    ))
(define (put-u32-le port c)
  (begin
    (put-u8 port (logand #xff c))
    (put-u8 port (logand #xff (ash c -8)))
    (put-u8 port (logand #xff (ash c -16)))
    (put-u8 port (logand #xff (ash c -24)))
    ))
(define (put-u64-le port c)
  (begin
    (put-u8 port (logand #xff c))
    (put-u8 port (logand #xff (ash c -8)))
    (put-u8 port (logand #xff (ash c -16)))
    (put-u8 port (logand #xff (ash c -24)))
    (put-u8 port (logand #xff (ash c -32)))
    (put-u8 port (logand #xff (ash c -40)))
    (put-u8 port (logand #xff (ash c -48)))
    (put-u8 port (logand #xff (ash c -56)))
    ))

(define (put-string port s)
  (string-for-each (lambda (c) (put-u8 port (char->integer c))) s))

(define (bytecode-put-u8 c)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (put-u8 bytecode-output-port c)
    (set-cdr! bytecode-current-section (+ 1 (cdr bytecode-current-section)))
    ))
(define (bytecode-put-u16 c)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (put-u16 bytecode-output-port c)
    (set-cdr! bytecode-current-section (+ 2 (cdr bytecode-current-section)))
    ))
(define (bytecode-put-u32 c)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (put-u32 bytecode-output-port c)
    (set-cdr! bytecode-current-section (+ 4 (cdr bytecode-current-section)))
    ))
(define (bytecode-put-u64 c)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (put-u64 bytecode-output-port c)
    (set-cdr! bytecode-current-section (+ 8 (cdr bytecode-current-section)))
    ))
(define (bytecode-put-u16-le c)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (put-u16-le bytecode-output-port c)
    (set-cdr! bytecode-current-section (+ 2 (cdr bytecode-current-section)))
    ))
(define (bytecode-put-u32-le c)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (put-u32-le bytecode-output-port c)
    (set-cdr! bytecode-current-section (+ 4 (cdr bytecode-current-section)))
    ))
(define (bytecode-put-u64-le c)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (put-u64-le bytecode-output-port c)
    (set-cdr! bytecode-current-section (+ 8 (cdr bytecode-current-section)))
    ))
(define (bytecode-put-string s)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (put-string bytecode-output-port s)
    (set-cdr! bytecode-current-section (+ (string-length s) (cdr bytecode-current-section)))
    ))


(define (bytecode-reserve len)
  (begin
    (if (null? bytecode-current-section) (errorp "bytecode-write-char called before bytecode-begin-section"))
    (let ((pos (ftell bytecode-output-port)))
      (do ((i 0 (1+ i))) ((>= i len)) (put-u8 bytecode-output-port 0))
      (set-cdr! bytecode-current-section (+ len (cdr bytecode-current-section)))
      pos
      )))
(define (bytecode-backpatch pos thunk)
  (begin
    (seek bytecode-output-port pos SEEK_SET)
    (thunk)
    (seek bytecode-output-port 0 SEEK_END)))

(define (bytecode-backpatch-u8 pos c) (bytecode-backpatch pos (lambda () (put-u8 bytecode-output-port c))))
(define (bytecode-backpatch-u16 pos c) (bytecode-backpatch pos (lambda () (put-u16 bytecode-output-port c))))
(define (bytecode-backpatch-u32 pos c) (bytecode-backpatch pos (lambda () (put-u32 bytecode-output-port c))))
(define (bytecode-backpatch-u64 pos c) (bytecode-backpatch pos (lambda () (put-u64 bytecode-output-port c))))
(define (bytecode-backpatch-u16-le pos c) (bytecode-backpatch pos (lambda () (put-u16-le bytecode-output-port c))))
(define (bytecode-backpatch-u32-le pos c) (bytecode-backpatch pos (lambda () (put-u32-le bytecode-output-port c))))
(define (bytecode-backpatch-u64-le pos c) (bytecode-backpatch pos (lambda () (put-u64-le bytecode-output-port c))))

(define label-patches vlist-null)
(define label-length 0)
(define (label-get-ref i) (vlist-ref label-patches (- (- label-length 1) i)))
(define (newlabel)
  (begin
    (set! label-patches (vlist-cons (cons 'NotEmitted #nil) label-patches))
    (set! label-length (+ 1 label-length))
    (- label-length 1)))
(define (bytecode-emit-label lab)
  (let ((l (label-get-ref lab))
        (pos (ftell bytecode-output-port)))
    (assert (equal? (car l) 'NotEmitted))
    (for-each (lambda (pos2) (begin
                               (assert (= (logand 3 (- pos (cdr pos2))) 0))
                               (bytecode-backpatch-u32-le (car pos2) (ash (- pos (cdr pos2)) -2))
                              )) (cdr l))
    (set-car! l 'Emitted)
    (set-cdr! l pos)
    ))

(define (bytecode-get-pos) (ftell bytecode-output-port))

(define (bytecode-emit-labref-with-pos lab frompos)
  (let ((l (label-get-ref lab))
        (pos (ftell bytecode-output-port)))
    (cond ((equal? (car l) 'NotEmitted)
           (begin
             (set-cdr! l (cons (cons pos frompos) (cdr l)))
             (bytecode-reserve 4)))
          ((equal? (car l) 'Emitted)
           (begin
             (assert (= (logand 3 (- (cdr l) frompos)) 0))
             (bytecode-put-u32-le (ash (- (cdr l) frompos) -2))))
          (else (assert #f)))))

(define (bytecode-emit-labref lab) (bytecode-emit-labref-with-pos lab (bytecode-get-pos)))

(define (bytecode-begin-section name)
  (begin
    (if (not (null? bytecode-current-section))
        (set! bytecode-sections (cons bytecode-current-section bytecode-sections)))
    (set! bytecode-current-section (cons name 0))
    ))
(define (bytecode-close-output)
  (begin
    (if (not (null? bytecode-current-section))
        (set! bytecode-sections (cons bytecode-current-section bytecode-sections)))
    (for-each (lambda (section) (begin
        (assert (string? (car section)))
        (assert (= (string-length (car section)) 4))
        (assert (number? (cdr section)))
        (put-string bytecode-output-port (car section))
        (put-u32 bytecode-output-port (cdr section))
     )) (reverse bytecode-sections))
    (put-u32 bytecode-output-port (length bytecode-sections))
    (put-string bytecode-output-port "Caml1999X023")
    (close-output-port bytecode-output-port)
    (set! bytecode-output-port #nil)
    ))

(define (bytecode-marshal value)
  (begin
    (bytecode-put-u32 #x8495A6BF) ; Intext_magic_number_big
    (bytecode-put-u32 0)          ; Unused
    (letrec*
        ((len 0)
         (size64 0)
         (lenpos (bytecode-reserve 8))
         (objcountpos (bytecode-reserve 8))
         (size64pos (bytecode-reserve 8))
         (loop
          (lambda (obj)
            (begin
              (cond ((integer? obj) (begin (bytecode-put-u8 #x3) (bytecode-put-u64 obj) (set! len (+ len 9))))
                    ((string? obj) (begin
                                     (bytecode-put-u8 #x15)
                                     (bytecode-put-u64 (string-length obj))
                                     (bytecode-put-string obj)
                                     (set! len (+ len (+ 9 (string-length obj))))
                                     (set! size64 (+ size64 (+ 1 (ash (+ (string-length obj) 8) -3))))
                                     ))
                    (else (let ((sz (length (cdr obj))))
                            (if (= sz 0)
                                (begin
                                  (bytecode-put-u8 #x8)
                                  (bytecode-put-u32 (car obj))
                                  (set! len (+ len 5)))
                                (begin
                                  (bytecode-put-u8 #x13)
                                  (bytecode-put-u64 (+ (car obj) (ash sz 10)))
                                  (set! len (+ len 9))
                                  (set! size64 (+ size64 (+ 1 sz)))
                                  (for-each loop (cdr obj))
                                  ))
                            ))
              )))))
      (loop value)
      (bytecode-backpatch-u64 lenpos len)
      (bytecode-backpatch-u64 objcountpos 0)
      (bytecode-backpatch-u64 size64pos size64)
      )))

(define globs #nil)
(define nglobs 0)
(define (newglob value)
  (begin
    (set! globs (cons value globs))
    (set! nglobs (+ 1 nglobs))
    (- nglobs 1)))
(define (slot-for-global) (newglob 0))
(define (bytecode-write-globals)
  (bytecode-marshal (cons 0 (reverse globs))))
(define prims #nil)
(define nprims 0)
(define (prim name)
  (if (equal? (string-ref name 0) #\%)
      (cons 'Internal (string->number (substring name 1)))
      (begin
        (set! prims (cons name prims))
        (set! nprims (+ 1 nprims))
        (cons 'C (- nprims 1)))))
(define (bytecode-write-prims)
  (for-each (lambda (name) (begin (bytecode-put-string name) (bytecode-put-u8 0))) (reverse prims)))

; Compression for the typical scheme where
; a few contiguous specializations are followed by the generic opcode.
; For example,
;    OP1, OP2, OP3, OP
; would use (bytecode-compressed-opcode 1 3 OP arg).
(define (bytecode-compressed-opcode range-start range-stop opcode arg)
  (if (and (<= range-start arg) (<= arg range-stop))
    (let* ((last-specialized-opcode (- opcode 1)))
      (bytecode-put-u32-le (+ last-specialized-opcode (- arg range-stop))))
    (begin
      (bytecode-put-u32-le opcode)
      (bytecode-put-u32-le arg)
    ))
)


(define (bytecode-CONSTINT n)
  (bytecode-compressed-opcode 0 3 CONSTINT n))

(define (bytecode-ACC n)
  (bytecode-compressed-opcode 0 7 ACC n))

(define (bytecode-ENVACC n)
  (bytecode-compressed-opcode 1 4 ENVACC n))

; The compression scheme for OFFSETCLOSURE is complex and
; changed in OCaml 4.12. Do try to compress it.
(define (bytecode-OFFSETCLOSURE n)
  (bytecode-put-u32-le OFFSETCLOSURE)
  (bytecode-put-u32-le n))

(define (bytecode-GETFIELD n)
  (bytecode-compressed-opcode 0 3 GETFIELD n))

(define (bytecode-SETFIELD n)
  (bytecode-compressed-opcode 0 3 SETFIELD n))

(define (bytecode-POP n)
  (bytecode-put-u32-le POP)
  (bytecode-put-u32-le n))

(define (bytecode-POP-to stacksize desired-stacksize)
  (assert (<= desired-stacksize stacksize))
  (if (< desired-stacksize stacksize)
      (bytecode-POP (- stacksize desired-stacksize))))

(define (bytecode-BRANCH-to label)
  (bytecode-put-u32-le BRANCH)
  (bytecode-emit-labref label))

; this one does not follow the standard compression scheme, it uses
; MAKEBLOCK, MAKEBLOCK1, MAKEBLOCK2, MAKEBLOCK3
(define (bytecode-MAKEBLOCK arity tag)
  (if (and (<= 1 arity) (<= arity 3))
      (bytecode-put-u32-le (+ MAKEBLOCK arity))
      (begin
        (bytecode-put-u32-le MAKEBLOCK)
        (bytecode-put-u32-le arity)))
  (bytecode-put-u32-le tag)
)

(define ACC 8)
(define PUSH 9)
(define POP 19)
(define ASSIGN 20)
(define ENVACC 25)
(define PUSH_RETADDR 31)
(define APPLY 32)
(define APPTERM 36)
(define RETURN 40)
(define RESTART 41)
(define GRAB 42)
(define CLOSURE 43)
(define CLOSUREREC 44)
(define OFFSETCLOSURE 48)
(define GETGLOBAL 53)
(define SETGLOBAL 57)
(define MAKEBLOCK 62)
(define GETFIELD 71)
(define SETFIELD 77)
(define BRANCH 84)
(define BRANCHIFNOT 86)
(define SWITCH 87)
(define PUSHTRAP 89)
(define POPTRAP 90)
(define C_CALL1 93)
(define C_CALL2 94)
(define C_CALL3 95)
(define C_CALL4 96)
(define C_CALL5 97)
(define C_CALLN 98)
(define CONSTINT 103)
(define ISINT 129)
(define BNEQ 132)
(define STOP 143)
(define RERAISE 146)
(define obj_tag (cdr (prim "caml_obj_tag")))

; bindings store for each name a definition, but also some visibility
; information, which indicate if this name was defined as "exported"
; by the current compilation unit ('Export), or is just available locally ('Local).
; In OCaml, this is the difference between `include M` and `open M`.
(define (bindings-get-err bindings name)
  (match (vhash-assoc name bindings)
    ((k . (viz . v)) v)
    (#f (errorp "Not found in env: " name))))

(define (bindings-replace name def bindings)
  (vhash-replace name (cons 'Export def) bindings))

(define (bindings-filter-map proc bindings)
  (vhash-filter-map
   (match-lambda* ((k (viz . v)) (let ((r (proc k v))) (if r (cons viz r) r))))
   bindings))

(define (bindings-only-exported bindings)
  (alist->vhash
   (filter (match-lambda ((k . (viz . v)) (match viz ('Export #t) ('Local #f))))
     (vlist->list bindings))))

(define (bindings-make-local bindings)
  (vhash-map (match-lambda* ((k (viz . v)) (cons 'Local v))) bindings))

(define (bindings-merge b1 b2)
  ; the binding added last is at the beginning of the list,
  ; so we fold-right to iterate from older to newer binding
  (vhash-fold-right vhash-replace b1 b2))


(define-immutable-record-type <env>
  (mkenv vars constrs fields modules)
  env?
  (vars env-get-vars env-with-vars)
  (constrs env-get-constrs env-with-constrs)
  (fields env-get-fields env-with-fields)
  (modules env-get-modules env-with-modules))

(define (env-replace-var env k v)
  (env-with-vars env (bindings-replace k v (env-get-vars env))))
(define (env-replace-constr env k v)
  (env-with-constrs env (bindings-replace k v (env-get-constrs env))))
(define (env-replace-field env k v)
  (env-with-fields env (bindings-replace k v (env-get-fields env))))
(define (env-replace-module env k v)
  (env-with-modules env (bindings-replace k v (env-get-modules env))))

(define empty-env (mkenv vlist-null vlist-null vlist-null vlist-null))

(define (env-zip-bindings f envs)
  (mkenv
   (f (map env-get-vars envs))
   (f (map env-get-constrs envs))
   (f (map env-get-fields envs))
   (f (map env-get-modules envs))))

(define (env-merge env1 env2)
  (env-zip-bindings
   (match-lambda ((b1 b2) (bindings-merge b1 b2)))
   (list env1 env2)))

(define (env-only-exported env)
  (env-zip-bindings
   (match-lambda ((b) (bindings-only-exported b)))
   (list env)))

(define (env-make-local env)
  (env-zip-bindings
   (match-lambda ((b) (bindings-make-local b)))
   (list env)))

(define (module-get-env mod)
  (match-let ((('VModule env) mod)) env))

(define (env-get-module env ld)
  (match ld
    (('Lident v)
     (bindings-get-err (env-get-modules env) v))
    (('Ldot ld uid)
     (bindings-get-err (env-get-modules (module-get-env (env-get-module env ld))) uid))))

(define (env-get-env-li env ld)
  (match ld
    (('Lident v)
     (cons env v))
    (('Ldot ld uid)
     (cons (module-get-env (env-get-module env ld)) uid))))

(define (env-get-var env ld)
  (match-let (((env . v) (env-get-env-li env ld)))
    (bindings-get-err (env-get-vars env) v)))
(define (env-get-constr env ld)
  (match-let (((env . v) (env-get-env-li env ld)))
    (bindings-get-err (env-get-constrs env) v)))
(define (env-get-field env ld)
  (match-let (((env . v) (env-get-env-li env ld)))
    (bindings-get-err (env-get-fields env) v)))

(define-immutable-record-type <var>
  (mkvar location funshape)
  var?
  (location var-get-location)
  (funshape var-get-funshape)
)

(define-immutable-record-type <constr>
  (mkconstr arity tag numconstrs)
  constr?
  (arity constr-get-arity)
  (tag constr-get-tag)
  (numconstrs constr-get-numconstrs))

(define-immutable-record-type <field>
  (mkfield index numfields)
  field?
  (index field-get-index)
  (numfields field-get-numfields))

(define (align-args funshape args)
  (define has-labelled-arg (any (lambda (arg) (not (equal? (car (cdr arg)) 'Nolabel))) args))
  (define (extract-first f l)
    (cond ((null? l) #nil)
          ((f (car l)) (cons (car l) (cdr l)))
          (else (let ((r (extract-first f (cdr l))))
                  (if (null? r) r (cons (car r) (cons (car l) (cdr r))))))
          ))
  (define (align shape args)
    (cond ((null? shape) (if (null? args) #nil (align (map (lambda (arg) (list 'Nolabel)) args) args)))
          ((null? args) #nil)
          ((equal? (car (car shape)) 'Nolabel)
           (let* ((r (extract-first (lambda (arg) (equal? (car (cdr arg)) 'Nolabel)) args))
                  (e (car (car r)))
                  (nargs (cdr r)))
             (cons e (align (cdr shape) nargs))))
          ((equal? (car (car shape)) 'Labelled)
           (let* ((s (car (cdr (car shape))))
                  (r (if has-labelled-arg
                         (extract-first (lambda (arg) (equal? (cdr arg) (list 'Labelled s))) args)
                         (extract-first (lambda (arg) (equal? (car (cdr arg)) 'Nolabel)) args)))
                  (e (car (car r)))
                  (nargs (cdr r)))
             (cons e (align (cdr shape) nargs))))
          ((equal? (car (car shape)) 'Optional)
           (let* ((s (car (cdr (car shape))))
                  (r (extract-first (lambda (arg) (or (equal? (cdr arg) (list 'Labelled s))
                                                      (equal? (cdr arg) (list 'Optional s)))) args))
                  (nargs (if (null? r) args (cdr r)))
                  (e (cond ((null? r) (lid->econstr "None" #nil))
                           ((equal? (car (cdr (car r))) 'Labelled) (lid->econstr "Some" (cons (car (car r)) #nil)))
                           (else (car (car r))))))
             (cons e (align (cdr shape) nargs))))
          ))
  ; (newline)(display funshape)(newline)(display args)(newline)

  (align funshape args))

(define rec-closure-step 2)

; Bytecode instructions expect "stack-relative" addresses,
; with 0 being the last slot on the stack.
(define (stack-relative stacksize absolute-pos)
  (- (- stacksize 1) absolute-pos))

(define (access-var location stacksize)
  (match location
    (('VarStack pos)
     (bytecode-ACC (stack-relative stacksize pos)))
    (('VarEnv pos)
     (bytecode-ENVACC (+ 1 pos)))
    (('VarRec pos)
     (bytecode-OFFSETCLOSURE pos))
  ))

(define (adjust-econstr-args args arity)
  (cond
   ((null? args)
    (assert (= arity 0))
    #nil)
   ((and (= arity 1) (> (length args) 1))
    (list (list 'EConstr (list 'Lident "") args)))
   ((and (> arity 1) (= (length args) 1))
    (match args
       ((('EConstr ('Lident "") args)) args)
       (_ args)))
   (else args)))

(define (adjust-pconstr-args args arity)
  (cond
   ((null? args)
    (assert (= arity 0))
    #nil)
   ((and (= arity 1) (> (length args) 1))
    (list (list 'PConstr (list 'Lident "") args)))
   ((and (> arity 1) (= (length args) 1))
    (match args
       ((('PWild)) (make-list arity (list 'PWild)))
       ((('PConstr ('Lident "") args)) args)
       (_ args)))
   (else args)))

(define (expand-record-pat env field-defs)
  (let* ((indexed-fields (map (lambda (fd) (index-field-def env fd)) field-defs))
         (numfields (fields-numfields env field-defs)))
    (map (lambda (index)
        (or (assoc-ref indexed-fields index) (list 'PWild))
      ) (range 0 numfields))
  ))

(define (show-env env)
  (display "Vars: ")
  (display (vlist->list (env-get-vars env)))
  (newline)
  (display "Constructors: ")
  (display (vlist->list (env-get-constrs env)))
  (newline)
  (display "Fields: ")
  (display (vlist->list (env-get-fields env)))
  (newline)
  (display "Modules: ")
  (display (vlist->list (env-get-modules env)))
  (newline))

(define-immutable-record-type <switch>
  (mkswitch consts blocks default nums)
  switch?
  (consts switch-get-consts switch-with-consts)
  (blocks switch-get-blocks switch-with-blocks)
  (default switch-get-default switch-with-default)
  (nums switch-get-nums switch-with-nums)
)

(define empty-switch (mkswitch #nil #nil #nil #nil))

(define (switch-cons-const sw const)
  (switch-with-consts sw (cons const (switch-get-consts sw))))
(define (switch-cons-block sw block)
  (switch-with-blocks sw (cons block (switch-get-blocks sw))))

(define no-nums (cons -1 -1))
(define (merge-nums nums cnums)
  (if (or (null? nums) (equal? nums cnums)) cnums no-nums))

(define (switch-no-nums sw)
  (switch-with-nums sw no-nums))
(define (switch-merge-nums sw nums)
  (switch-with-nums sw (merge-nums (switch-get-nums sw) nums)))

(define-immutable-record-type <switch-const>
  (mkswitch-const tag rhs)
  switch-const?
  (tag switch-const-get-tag)
  (rhs switch-const-get-rhs))

(define-immutable-record-type <switch-block>
  (mkswitch-block tag arity vars rhs)
  switch-block?
  (tag switch-block-get-tag)
  (arity switch-block-get-arity)
  (vars switch-block-get-vars)
  (rhs switch-block-get-rhs))

; Pattern-matching compilation is done using "matrix decomposition".
; A matrix represents an ordered list of pattern-matching clauses that
; simultaneously match on several argument variables (rather than arbitrary expressions).
; The right-hand-side of each clause is not only an expression, but also a list
; of bindings of pattern variables to argument variables.
;
; One matrix corresponding to
;   match x,  y,  z with
;      |  p1 as v, q1, r1 -> e1
;      |  p2,      q2 as w, r2 -> e2
; has the "arguments" list (args)
;   (list x y z)
; and the "matrix" of clauses (m)
;   (list
;    (list (list p1 q1 r1) (list (cons v x)) e1)
;    (list (list p2 q2 r2) (list (cons w y)) e2))
; The "arity" of a matrix is the size of its (args)
; list, and the number of columns of the matrix --
; the two quantities always remain equal.
;
; The output of the matrix decomposition is a "matching tree"
; (constructors use the 'MT prefix), which is itself turned
; into lower code by the lower-matching-tree function.
;
; Note: matrix rows are represented as fixed-sized lists
; (one element for the patterns, one for bindings, one for the right-hand-side)
; instead of cons-pairs, as further specialized representations
; are used during the transformation at higher arities
; (4 for non-empty rows, 5 for non-empty rows with beheaded simple first pattern)
; and cons-pairs quickly become inconvenient at higher arities.
;
; Note: in practice we do not use expressions as actions during matrix decomposition,
; we represent each action by just its index in the initial clauses. This lets us
; detect which actions are duplicated by the matching process, and turn those into exits
; to avoid code duplication.
(define (lower-match env istail arg h)
  (match-let* (
     (patterns (map car h))
     (actions (map cdr h))
     (pattern-vars (map pat-vars patterns))
     (actions
      (map (lambda (pat-vars e)
             (lower-expr (local-vars env pat-vars) istail e)
       ) pattern-vars actions))
     (action-numbers (range 0 (length actions)))
     (args (list arg))
     (matrix
      ; a list of clauses is turned into a matrix of arity 1
      (map (lambda (p action-number) (list (list p) #nil action-number)) patterns action-numbers))
     (matching-tree
      (match-decompose-matrix env args matrix))
     (action-frequencies
      (compute-action-frequencies action-numbers matching-tree))
     ((exit-defs . dedup-actions)
      (deduplicate-actions
       (map
        (lambda (vars e freq) (list vars e freq))
         pattern-vars actions action-frequencies)))
     (code
      (lower-matching-tree env matching-tree dedup-actions))
  )
  (if (null? exit-defs)
      code
      (list 'LLetexits exit-defs code))
))

(define (pat-vars p)
  (let loop ((p p) (acc #nil))
    (match p
      (('PVar v) (cons v acc))
      (('PAs p v) (loop p (cons v acc)))
      (('PWild) acc)
      (('PInt _) acc)
      (('PConstr c ps) (fold-right loop acc ps))
      (('POr p1 p2)
       (let* ((vars1 (loop p1 #nil))
              (vars2 (loop p2 #nil)))
         (assert (equal? (sort vars1 <=) (sort vars2 <=)))
         (append vars1 acc)
       ))
      (('PRecord field-defs)
       (fold-right loop acc (map cdr field-defs)))
      (('PFullRecord field-pats)
       (fold-right loop acc field-pats))
)))

(define (match-decompose-matrix env args m)
  ; (display "matrix") (display args) (newline)
  ; (for-each (lambda (r) (display r) (newline)) m)
  (assert (or (null? m) (equal? (length args) (length (car (car m))))))
  (match (cons args m)
    ((_ . #nil)
     ; no clauses: this matrix rejects all inputs
     (list 'MTFailure))
    ((#nil . ((#nil bindings e) . _))
     ; at least one clause of arity 0: matching (on nothing)
     ; always succeeds and goes to this right-hand-side.
     (list 'MTAction bindings e))
    (((arg . args) . m)
     ; arity > 0; we represent m with a distinguished head column
     ; (first column, other columns, bindings, action)
     (let*
      ((m (map (match-lambda (((p . ps) bindings e) (list p ps bindings e))) m))
       (m (simplify-matrix env arg m))
       (groups (split-matrix-in-groups env m))
       (matching-tree (match-decompose-groups env arg args groups)))
      matching-tree
     ))
    ))

(define (simplify-matrix env arg m)
  (append-map (lambda (row) (simplify-row env arg row)) m))

; Rows of a "simplified (non-empty) matrix" are of the form
; (head head-subpatterns other-patterns bindings action)
;
; A head is either #nil, for the wildcard pattern, or a "strong head"
; representing the head constructor of the values being matched.
;
; This function returns a list of rows, as or-patterns may simplify
; into several-rows. For example,
;   ((Nil | Cons (_, _)) ps bindings e
; simplifies into two rows
;   Nil #nil    ps bindings e
;   Cons (_ _) ps bindings e
(define (simplify-row env arg row)
  (match-let (((p ps bindings e) row))
   (match p
    (('PVar v)
     (let* ((p (list 'PAs (list 'PWild) v)))
     (simplify-row env arg (list p ps bindings e))))
    (('PAs p v)
     (let* ((bindings (cons (cons v arg) bindings)))
     (simplify-row env arg (list p ps bindings e))))
    (('PRecord field-defs)
     (let* ((p (list 'PFullRecord (expand-record-pat env field-defs))))
     (simplify-row env arg (list p ps bindings e))))
    (('PWild)
     (list (list #nil #nil ps bindings e)))
    (('PInt n)
     (list (list (list 'HPInt n) #nil ps bindings e)))
    (('PConstr c l)
     (let* ((arity (constr-get-arity (env-get-constr env c)))
            (l (adjust-pconstr-args l arity))
            (arity (length l)))
       (list (list (list 'HPConstr c arity) l ps bindings e))))
    (('PFullRecord field-pats)
     (list (list (list 'HPRecord (length field-pats)) field-pats ps bindings e)))
    (('POr p1 p2)
     (append
      (simplify-row env arg (list p1 ps bindings e))
      (simplify-row env arg (list p2 ps bindings e))))
)))

(define (pattern-head-arity h)
  (match h
    (#nil 0)
    (('HPInt _) 0)
    (('HPConstr c arity) arity)
    (('HPRecord arity) arity)
  ))

(define (omegas-for-pattern-head h)
  (make-list (pattern-head-arity h) (list 'PWild)))

(define (args-for-pattern-head arg h)
  (map (lambda (i) (string-append arg "#" (number->string i))
     ) (range 0 (pattern-head-arity h))))

(define (split-matrix-in-groups env simplified-matrix)
  (let*
      (; first we compute all groups present in the matrix
       (groups
        (fold (lambda (row groups)
                (match-let (((h hps ps bindings e) row))
                 (vhash-replace h #nil groups))
               ) vlist-null simplified-matrix))
       ; then we populate all the groups by adding rows with the corresponding head
       (groups
        (fold-right (lambda (row groups)
                (match-let (((h hps ps bindings e) row))
                 (assert (pair? (vhash-assoc h groups)))
                 (if (not (null? h))
                   (match-let (((_ . group-rows) (vhash-assoc h groups))
                               (row (list (append hps ps) bindings e)))
                     (vhash-replace h (cons row group-rows) groups))
                   (begin
                     (assert (null? hps))
                     ; in the wildcard case, we add the row to the default group
                     ; (representing any value whose head is not among the other groups)
                     ; *and* to each group (their values also match a wildcard)
                     ; this is why we need to have a pre-computed list of all groups
                     (vhash-fold (lambda (h group-rows groups)
                       (let ((row (list (append (omegas-for-pattern-head h) ps) bindings e)))
                         (vhash-replace h (cons row group-rows) groups))
                       ) vlist-null groups))
                   ))
               ) groups simplified-matrix))
       )
    groups))

(define (match-decompose-groups env arg args groups)
  (list
   'MTSwitch
   arg
   (vhash-map (lambda (h submatrix)
       (let* ((newargs (args-for-pattern-head arg h))
              (matching-tree (match-decompose-matrix env (append newargs args) submatrix)))
       (cons newargs matching-tree))
     ) groups)
))

(define (compute-action-frequencies action-numbers matching-tree)
  (assert (equal? action-numbers (range 0 (length action-numbers))))
  (let ((freqs (make-vector (length action-numbers) 0)))
  (let iter ((matching-tree matching-tree))
    (match matching-tree
      (('MTFailure) #f)
      (('MTAction bindings act)
       (vector-set! freqs act (+ 1 (vector-ref freqs act))))
      (('MTSwitch arg groups)
       (vlist-for-each (match-lambda ((h . (vars . group-tree)) (iter group-tree))) groups))
  ))
  (vector->list freqs)
))

(define (deduplicate-actions actions)
  (fold-right (lambda (action i acc)
      (match-let* (
         ((vars e freq) action)
         ((exit-defs . actions) acc)
      )
      (if (or (<= freq 1) (substituable? e))
        (cons exit-defs (cons e actions))
        (let* (
           (exit (string-append "act#" (number->string i)))
           (exit-def (list exit vars e))
           (exit-action (list 'LExit exit (map lid->lvar vars)))
         )
         (cons (cons exit-def exit-defs) (cons exit-action actions)))
    ))) (cons #nil #nil) actions (range 0 (length actions))))

(define (lower-matching-tree env matching-tree actions)
  (match matching-tree
    (('MTFailure) (list 'LMatchFailure))
    (('MTAction bindings action-number)
     ; bindings were accumulated into a list during decomposition,
     ; so the earliest binding is at the end of the list. We use 'fold' so that
     ; it ends up at the beginning of the resulting expression.
     (fold (match-lambda* (((v . arg) e)
             ; note: this Let is a variable rebinding (let x = y in ...),
             ; with special support in the code generator
             (list 'LLet v (lid->lvar arg) e)
        )) (list-ref actions action-number) bindings))
    (('MTSwitch arg groups) (lower-match-groups env arg groups actions))
))

(define (lower-match-groups env arg groups actions)
  (let* (; separate the wildcard group from the groups with strong heads
         (default-matching-tree
           (let ((r (vhash-assoc #nil groups)))
             ; the returned pair, if any, binds the head #nil to
             ; a cons-list of new arguments (also #nil) and the
             ; default matching tree.
             (if (pair? r) (cddr r) #nil)))
         (groups (vhash-delete #nil groups))
         (sw (if (null? default-matching-tree)
                 empty-switch
                 (switch-with-default empty-switch (cons #nil
                   (lower-matching-tree env default-matching-tree actions)))))
         (sw (vhash-fold (lambda (h group sw)
                (match-let* (((group-vars . group-matching-tree) group)
                             (group-code (lower-matching-tree env group-matching-tree actions)))
                (match h
                  (('HPInt n)
                   (switch-cons-const sw (mkswitch-const n group-code)))
                  (('HPRecord arity)
                   (switch-cons-block sw (mkswitch-block 0 arity group-vars group-code)))
                  (('HPConstr c arity)
                   ; TODO: instead of passing argument variables to the switch compiler,
                   ; we could lower the field-access logic by binding the new-args
                   ; with LGetField access.
                   (let* ((cdef (env-get-constr env c))
                          (nums (constr-get-numconstrs cdef))
                          (tag (constr-get-tag cdef))
                          (sw (switch-merge-nums sw nums)))
                   (if (equal? arity 0)
                       (switch-cons-const sw (mkswitch-const tag group-code))
                       (switch-cons-block sw (mkswitch-block tag arity group-vars group-code)))))
                ))) sw groups)))
  (list 'LSwitch (lid->lvar arg) sw)))

(define (lower-expr env istail expr)
  (let ((lower-tail (lambda (e) (lower-expr env istail e)))
        (lower-notail (lambda (e) (lower-expr env #f e))))
  (match expr
    (('EVar ld)
     (match (var-get-location (env-get-var env ld))
        (('VarLocal v)
         (assert (equal? ld (list 'Lident v)))
         (list 'LVar v))
        (('VarGlobal id)
         (list 'LGlobal id))
      ))
    (('EConstant c)
     (match c
       (('CInt n)
        (list 'LConst n))
       (('CUnit)
        (list 'LConst 0))
       (('CString str)
        (list 'LGlobal (newglob str)))))
    (('EIf e1 e2 e3)
     (list
      'LIf
      (lower-notail e1)
      (lower-tail e2)
      (lower-tail e3)
     ))
    (('EChain e1 e2)
     (list
      'LChain
      (lower-notail e1)
      (lower-tail e2)
      ))
    (('EConstr name args)
     (match-let* ((($ <constr> arity tag nums) (env-get-constr env name))
                  (args (adjust-econstr-args args arity))
                  (args (map lower-notail args)))
       (cond
        ((null? args)
         (list 'LConst tag))
        ((= (car nums) -2)
         (list 'LBlock 0 (cons (list 'LConst tag) args)))
        (else
         (list 'LBlock tag args)))))
    (('EGetfield e f)
     (list
      'LGetfield
      (lower-notail e)
      (lower-field-index env f)
     ))
    (('ESetfield e1 f e2)
     (list
      'LSetfield
      (lower-notail e1)
      (lower-field-index env f)
      (lower-notail e2)
     ))
    (('ERecord l)
        (let* ((size (length l))
               (lf (map (lambda (fe) (lower-field-def env fe)) l))
               (sf (sort lf (lambda (fe1 fe2) (< (car fe1) (car fe2)))))
               (es (map cdr sf)))
          (for-each (lambda (fe i) (assert (= (car fe) i))) sf (range 0 size))
          (list 'LBlock 0 es)))
    (('ERecordwith e l)
     (let* ((size (fields-numfields env l))
            (e (lower-notail e))
            (lf (map (lambda (fe) (lower-field-def env fe)) l))
            (var "record#with")
            (es (map (lambda (i)
                       (let* ((p (find (lambda (fe) (= (car fe) i)) lf)))
                         (if (pair? p)
                             (cdr p)
                             (list 'LGetfield (lid->lvar var) i)))
                       ) (range 0 size)))
            )
       (assert (> size 0))
       (list 'LLet var e (list 'LBlock 0 es))))
    (('EApply f args)
      (match-let* ((($ <var> f-location f-shape) (env-get-var env f))
                   (args (align-args f-shape args))
                   (args (map lower-notail args))
                   (e (lower-notail (list 'EVar f))))
       (if istail
           (list 'LTailApply e args)
           (list 'LApply e args))))
    (('EMatch e h)
     (list 'LLet "match#arg" (lower-notail e)
             (lower-match (local-var env "match#arg") istail "match#arg" h)))
    (('ETry e h)
     (let* ((e (lower-notail e))
            (var "try#exn"))
       (list 'LCatch e var
         (lower-expr (local-var env var) istail
          (list 'EMatch (lid->evar var)
           (append h (list
             (cons (list 'PWild) (list 'EReraise (lid->evar var))))))))))
    (('EReraise e)
     (list 'LReraise (lower-notail e)))
    (('ELet rec-flag bindings body)
       (if rec-flag
         (lower-letrec env istail bindings body)
         (lower-let env istail bindings body)
        ))
    (('ELambda args fun)
     (match-let* (((args shape fun) (lower-function env args fun)))
      (list 'LLetfun  "lambda#" args fun (lid->lvar "lambda#"))))
    (('ELetOpen m e)
       (let* ((menv (env-get-module env m)))
         (lower-expr (env-open env menv) istail e)))
    (('ELetExits bindings body)
     (list
      'LLetexits
      (map (match-lambda
            ((('PVar exit) . ('ELambda args fun))
             (let ((vars (map (match-lambda ((= arg-get-pat ('PVar arg)) arg)) args)))
             (list exit vars (lower-expr (local-vars env vars) istail fun))))
            ((('PVar exit) . e)
             (list exit #nil (lower-tail e)))
       ) bindings)
      (lower-tail body)))
    (('EExit id args)
     (list
      'LExit
      id
      (map (match-lambda ((arg . ('Nolabel))
             (lower-notail arg)
        )) args)))
)))

(define (local-var-with-shape env v shape)
  (env-replace-var env v (mkvar (list 'VarLocal v) shape)))
(define (local-var env v)
  (local-var-with-shape env v #nil))
(define (local-vars env vars)
  (fold (lambda (var env) (local-var env var)) env vars))

(define (lower-letrec env istail bindings body)
  (let* ((bindings
          (map (match-lambda
                ((('PVar v) . ('ELambda args fun))
                 (cons v (lower-function-prelude args fun)))
             ) bindings))
         (env
          (fold (match-lambda*
              (((v args shape fun) env) (local-var-with-shape env v shape))
            ) env bindings))
         (bindings
          (map (match-lambda
                ((v args shape fun) (list v args (lower-function-body env args fun)))
             ) bindings))
         (body
          (lower-expr env istail body)))
    (list 'LLetrecfun bindings body)))

(define (lower-let env istail bindings body)
  ; HACK: sequential let!
  (match bindings
    (#nil (lower-expr env istail body))
    ((binding . rest)
     (let ((lower-rest (lambda (env) (lower-let env istail rest body))))
       (match binding
         ((('PVar v) . (and e ('ELambda args fun)))
          (match-let* (((args shape fun) (lower-function env args fun))
                       (env (local-var-with-shape env v shape)))
            (list 'LLetfun v args fun (lower-rest env))))
         ((('PVar v) . e)
          (let* ((e (lower-expr env #f e))
                 (env (local-var env v)))
            (list 'LLet v e (lower-rest env))))
         ((('PWild) . e)
          (let* ((e (lower-expr env #f e)))
            (list 'LChain e (lower-rest env))))
         ((p . e)
          ; here we interrupt binding traversal
          ; to go back to the general lower-expr
          (lower-expr env istail
            (list
             'EMatch
             e
             (list (cons p (list 'ELet #f rest body))))))
      )))
  ))

(define (lower-field-index env f)
  (field-get-index (env-get-field env f)))

(define (lower-field-def env field-def)
  (match-let* (((index . expr) (index-field-def env field-def)))
    (cons index (lower-expr env #f expr))))

(define (index-field-def env fe)
  (match-let* (((f . e) fe)
               (($ <field> index numfields) (env-get-field env f)))
    (cons index e)))

(define (fields-numfields env fes)
  (match-let* (((f . e) (car fes))
               (($ <field> index numfields) (env-get-field env f)))
    numfields))

(define (lower-handler env istail h)
  (map (match-lambda ((p . e) (cons p (lower-expr env istail e)))) h))

(define (lower-function env args body)
  (match-let* (((args shape body) (lower-function-prelude args body))
               (body (lower-function-body env args body)))
    (list args shape body)))

(define (lower-function-body env args body)
  (let* ((env (local-vars env args)))
  (lower-expr env #t body)))

(define (lower-function-prelude args body)
  (let* ((arity (length args))
         (shape (map arg-get-label args))
         (arg-names (map lower-arg-name args (range 0 arity)))
         (body (lower-function-prelude-aux args arg-names body)))
    (list arg-names shape body)))

(define (lower-function-prelude-aux args arg-names basebody)
  (fold-right (lambda (arg name body) (match arg
     (($ <arg> pat ('Optional label) ('Some default))
      (lower-arg-default label default body))
     (($ <arg> pat _ ('None))
      (lower-arg-pat pat name body))))
   basebody args arg-names))

(define (lower-arg-name a i)
  (match (arg-get-pat a)
     (('PVar v)
      v)
     (_
      (string-append "arg#" (number->string i)))))

(define (lower-arg-default name default body)
  (let* ((noneline (cons (lid->pconstr "None" #nil) default))
         (someline (cons (lid->pconstr "Some" (list (lid->pvar name)))
                         (lid->evar name)))
         (default-expr (list 'EMatch (lid->evar name) (list noneline someline))))
    (list 'ELet #f (list (cons (lid->pvar name) default-expr)) body)))

(define (lower-arg-pat pat name body)
  (match pat
   (('PVar _)
    body)
   (_
    (list 'EMatch (lid->evar name) (list (cons pat body))))))

(define (lower-switch env istail sw)
  (match-let*
   ((($ <switch> consts blocks default nums) sw)
    (consts
      (map (match-lambda
            (($ <switch-const> tag rhs)
             (mkswitch-const tag
               (lower-expr env istail rhs)))
         ) consts))
    (blocks
      (map (match-lambda
            (($ <switch-block> tag arity vars rhs)
             (mkswitch-block tag arity vars
               (lower-expr (local-vars env vars) istail rhs)))
        ) blocks))
    (default
      ((match-lambda
        (#nil #nil)
        ((var . rhs) (cons var (lower-expr (local-var env var) istail rhs)))
        ) default)))
   (mkswitch consts blocks default nums)))

(define-immutable-record-type <compenv>
  (mkcompenv vars traps exits)
  compenv?
  (vars compenv-get-vars compenv-with-vars)
  (traps compenv-get-traps compenv-with-traps)
  (exits compenv-get-exits compenv-with-exits)
)

(define compenv-empty (mkcompenv vlist-null #nil vlist-null))

(define (compenv-replace-var env k v)
  (compenv-with-vars env (vhash-replace k v (compenv-get-vars env))))
(define (compenv-get-var env var)
  (match (vhash-assoc var (compenv-get-vars env))
     ((_ . pos) pos)))

(define (compenv-push-trap env stacksize-before stacksize-after)
  (compenv-with-traps env
   (cons (list stacksize-before stacksize-after)
    (compenv-get-traps env))))

(define (compenv-add-exit env exit exit-info)
  (compenv-with-exits env
    (vhash-replace exit exit-info
      (compenv-get-exits env))))
(define (compenv-get-exit env exit)
  (match (vhash-assoc exit (compenv-get-exits env))
     ((_ . info) info)))

(define-immutable-record-type <exit-info>
  (make-exit-info label arity compenv stacksize)
  exit?
  (label exit-info-get-label)
  (arity exit-info-get-arity)
  (compenv exit-info-get-compenv)
  (stacksize exit-info-get-stacksize)
)

(define (compile-high-expr env istail expr)
  (compile-expr compenv-empty 0
    (lower-expr env istail expr)))

(define (substituable? expr)
  (match expr
   (((or 'LVar 'LGlobal 'LConst) . _)
    #t)
   (((or 'LBlock
        'LGetfield 'Setfield
        'LApply 'LTailApply
        'LIf 'LChain 'LSwitch
        'LReraise 'LCatch
        'LLet 'LLetfun 'LLetrecfun
        'LLetexits 'LExit
     ) . _)
    #f)
))

(define (compile-expr env stacksize expr)
  ; (display "compile-expr") (newline)
  ; (show-env env)(newline)
  ; (display stacksize)(newline)
  ; (display expr)(newline)
  (match expr
    (('LVar v)
     (match (compenv-get-var env v)
        (('VarSubst env expr)
         (compile-expr env stacksize expr))
        (loc
         (access-var loc stacksize))))
    (('LGlobal id)
     (bytecode-put-u32-le GETGLOBAL)
     (bytecode-put-u32-le id))
    (('LConst n)
     (if (and (<= -1073741824 n) (< n 1073741823))
         (bytecode-CONSTINT n)
         (begin
           (bytecode-put-u32-le GETGLOBAL)
           (bytecode-put-u32-le (newglob n)))))
    (('LBlock tag args)
     (compile-args env stacksize args)
     (bytecode-MAKEBLOCK (length args) tag))
    (('LGetfield e i)
     (compile-expr env stacksize e)
     (bytecode-GETFIELD i))
    (('LSetfield e1 i e2)
     (compile-expr env stacksize e2)
     (bytecode-put-u32-le PUSH)
     (compile-expr env (+ 1 stacksize) e1)
     (bytecode-SETFIELD i))
    (('LTailApply e args)
     (let* ((nargs (length args)))
       (compile-args env stacksize args)
       (bytecode-put-u32-le PUSH)
       (compile-expr env (+ stacksize nargs) e)
       (bytecode-put-u32-le APPTERM)
       (bytecode-put-u32-le nargs)
       (bytecode-put-u32-le (+ stacksize nargs))))
    (('LApply e args)
     (let* ((nargs (length args))
            (lab (newlabel)))
       (bytecode-put-u32-le PUSH_RETADDR)
       (bytecode-emit-labref lab)
       (compile-args env (+ stacksize 3) args)
       (bytecode-put-u32-le PUSH)
       (compile-expr env (+ stacksize (+ 3 nargs)) e)
       (bytecode-put-u32-le APPLY)
       (bytecode-put-u32-le nargs)
       (bytecode-emit-label lab)))
    (('LIf e1 e2 e3)
      (let* ((lab1 (newlabel))
             (lab2 (newlabel)))
        (compile-expr env stacksize e1)
        (bytecode-put-u32-le BRANCHIFNOT)
        (bytecode-emit-labref lab1)
        (compile-expr env stacksize e2)
        (bytecode-BRANCH-to lab2)
        (bytecode-emit-label lab1)
        (compile-expr env stacksize e3)
        (bytecode-emit-label lab2)))
    (('LChain e1 e2)
     (compile-expr env stacksize e1)
     (compile-expr env stacksize e2))
    (('LSwitch e sw)
     (compile-expr env stacksize e)
     (compile-switch env stacksize sw))
    (('LReraise e)
     (compile-expr env stacksize e)
     (bytecode-put-u32-le RERAISE))
    (('LMatchfailure)
     ; TODO: we could actually raise a Match_failure exception (no location)
     (bytecode-put-u32-le STOP))
    (('LCatch body exnvar handler)
     (let* ((lab1 (newlabel))
            (lab2 (newlabel))
            (stacksize-before-trap stacksize)
            (stacksize-after-trap (+ stacksize 4))
            )
       (bytecode-put-u32-le PUSHTRAP)
       (bytecode-emit-labref lab1)
       (compile-expr
        (compenv-push-trap env stacksize-before-trap stacksize-after-trap)
        stacksize-after-trap body)
       (bytecode-put-u32-le POPTRAP)
       (bytecode-BRANCH-to lab2)
       (bytecode-emit-label lab1)
       (compile-bind-var env stacksize exnvar handler)
       (bytecode-emit-label lab2)))
    (('LLet var e body)
     (if (substituable? e)
         (compile-expr
          (compenv-replace-var env var (list 'VarSubst env e))
          stacksize body)
         (begin
           (compile-expr env stacksize e)
           (compile-bind-var env stacksize var body))))
    (('LLetfun f args fun body)
     (compile-fundef env stacksize args fun)
     (compile-bind-var env stacksize f body))
    (('LLetrecfun bindings body)
     (let* ((nenv (compile-recfundefs env stacksize bindings)))
       (compile-expr nenv (+ stacksize (length bindings)) body)
       (bytecode-POP (length bindings))))
    (('LLetexits exit-defs body)
     (compile-letexits env stacksize exit-defs body))
    (('LExit exit args)
     (compile-exit env stacksize exit args))
))

(define (compile-expr-list env stacksize l)
  (if (not (null? l))
      (begin
        (compile-expr env stacksize (car l))
        (if (not (null? (cdr l)))
            (begin
              (bytecode-put-u32-le PUSH)
              (compile-expr-list env (+ stacksize 1) (cdr l))))
        )))

(define (compile-args env stacksize l) (compile-expr-list env stacksize (reverse l)))

(define (stack-var env var pos)
  (compenv-replace-var env var (list 'VarStack pos)))

(define (compile-bind-fields env stacksize vars e)
  (match-let (((i j env) (fold
             (match-lambda* ((var (i j env))
               (if (null? var)
                   (list (+ i 1) j env)
                   (let* ((nenv (stack-var env var (+ stacksize j))))
                     (bytecode-ACC j)
                     (bytecode-put-u32-le GETFIELD)
                     (bytecode-put-u32-le i)
                     (bytecode-put-u32-le PUSH)
                     (list (+ i 1) (+ j 1) nenv)))
               )) (list 0 0 env) vars)))
    (compile-expr env (+ stacksize j) e)
    (bytecode-POP (+ j 1))
  ))

(define (compile-bind-var env stacksize var e)
  (if (null? var)
      (compile-expr env stacksize e)
      (begin
        (bytecode-put-u32-le PUSH)
        (compile-expr (stack-var env var stacksize) (+ stacksize 1) e)
        (bytecode-POP 1))))

(define (compile-bind-var-pushed env stacksize var e)
  (if (null? var)
      (begin
        (bytecode-POP 1)
        (compile-expr env (- stacksize 1) e))
      (begin
        (compile-expr (stack-var env var (- stacksize 1)) stacksize e)
        (bytecode-POP 1))))

(define (compile-switch env stacksize sw)
  (match-let ((($ <switch> consts blocks default nums) sw))
    (if (and (null? consts) (null? blocks))
        (compile-bind-var env stacksize (car default) (cdr default))
        (if (or (null? nums) (< (car nums) 0))
            ; The matched constructors do not carry representation information
            ; from their type declaration. This typically occurs when matching on
            ; integer literals, or exceptions in the (try .. with ...) desugaring.
            ;
            ; Inefficient compilation using if and Obj.tag.
            (let* ((labblock (newlabel))
                   (labdef (newlabel))
                   (endlab (newlabel))
                   (exntype (and (pair? nums) (= (car nums) -2))))
              (bytecode-put-u32-le PUSH)
              (bytecode-put-u32-le ISINT)
              (bytecode-put-u32-le BRANCHIFNOT)
              (bytecode-emit-labref labblock)
              (bytecode-ACC 0)
              (for-each (match-lambda (($ <switch-const> i e)
                          (let* ((lab (newlabel)))
                            (bytecode-put-u32-le BNEQ)
                            (bytecode-put-u32-le i)
                            (bytecode-emit-labref lab)
                            (bytecode-POP 1)
                            (compile-expr env stacksize e)
                            (bytecode-BRANCH-to endlab)
                            (bytecode-emit-label lab)
                            )))
                        consts)
              (bytecode-BRANCH-to labdef)
              (bytecode-emit-label labblock)
              (bytecode-ACC 0)
              (if exntype
                  (begin
                    (bytecode-put-u32-le GETFIELD)
                    (bytecode-put-u32-le 0))
                  (begin
                    (bytecode-put-u32-le C_CALL1)
                    (bytecode-put-u32-le obj_tag)))
              (for-each (match-lambda (($ <switch-block> i _ l e)
                          (let* ((lab (newlabel))
                                 (l (if exntype (cons #nil l) l)))
                            (bytecode-put-u32-le BNEQ)
                            (bytecode-put-u32-le i)
                            (bytecode-emit-labref lab)
                            (compile-bind-fields env (+ stacksize 1) l e)
                            (bytecode-BRANCH-to endlab)
                            (bytecode-emit-label lab)
                            )))
                        blocks)
              (bytecode-emit-label labdef)
              (if (pair? default)
                  (compile-bind-var-pushed env (+ stacksize 1) (car default) (cdr default))
                  (bytecode-put-u32-le STOP))
              (bytecode-emit-label endlab))
            (let* ((numconsts (car nums))
                   (numblocks (cdr nums))
                   (endlab (newlabel))
                   (defaultlab (newlabel))
                   (constslabs (map (lambda (c) (cons (newlabel) c)) consts))
                   (blockslabs (map (lambda (b) (cons (newlabel) b)) blocks))
                   (basepos (+ (bytecode-get-pos) 8))
                   )
              (bytecode-put-u32-le SWITCH)
              (bytecode-put-u16-le numconsts)
              (bytecode-put-u16-le numblocks)
              (for-each (lambda (i)
                          (let* ((p (find (lambda (p) (= i (switch-const-get-tag (cdr p)))) constslabs))
                                 (lab (if (pair? p) (car p) defaultlab)))
                            (bytecode-emit-labref-with-pos lab basepos)
                          )) (range 0 numconsts))
              (for-each (lambda (i)
                          (let* ((p (find (lambda (p) (= i (switch-block-get-tag (cdr p)))) blockslabs))
                                 (lab (if (pair? p) (car p) defaultlab)))
                            (bytecode-emit-labref-with-pos lab basepos)
                          )) (range 0 numblocks))
              (for-each (match-lambda ((lab . ($ <switch-const> n e))
                          (bytecode-emit-label lab)
                          (compile-expr env stacksize e)
                          (bytecode-BRANCH-to endlab)
                          )) constslabs)
              (for-each (match-lambda ((lab . ($ <switch-block> _ _ l e))
                          (bytecode-emit-label lab)
                          (bytecode-put-u32-le PUSH)
                          (compile-bind-fields env (+ stacksize 1) l e)
                          (bytecode-BRANCH-to endlab)
                          )) blockslabs)
              (bytecode-emit-label defaultlab)
              (if (pair? default)
                  (compile-bind-var env stacksize (car default) (cdr default))
                  (bytecode-put-u32-le STOP)) ; match failure
              (bytecode-emit-label endlab)
              ))
    )))

(define (compile-letexits env stacksize exit-defs body)
  (let* (
      (endlab (newlabel))
      (exit-labels (map (lambda (def) (newlabel)) exit-defs))
      (exit-names (map car exit-defs))
      (exit-varss (map cadr exit-defs))
      (exit-bodies (map caddr exit-defs))
      (exit-infos
       (map (lambda (exit-label exit-vars)
               (compute-exit-info env stacksize exit-label exit-vars)
         ) exit-labels exit-varss))
      (body-env
       (fold (lambda (name info env) (compenv-add-exit env name info))
           env exit-names exit-infos))
      (max-arity
       (fold (lambda (info max-arity) (max (exit-info-get-arity info) max-arity))
           0 exit-infos))
    )
    ; Each exit needs an "argument space" of size exit-arity reserved on the stack.
    ; Jumping to one of the exits leaves the scope were those exits are available,
    ; so we will jump to at most one of these exit.
    ; We reserve overlapping space for their arguments, of size max-arity.
    (bytecode-CONSTINT 0)
    (for-each (lambda (i)
        (bytecode-put-u32-le PUSH)
      ) (range 0 max-arity))
    (compile-expr body-env (+ stacksize max-arity) body)
    (bytecode-POP max-arity)
    (bytecode-BRANCH-to endlab)
    (for-each (match-lambda* ((($ <exit-info> exit-label exit-arity exit-env exit-stacksize) exit-body)
         (bytecode-emit-label exit-label)
         (compile-expr exit-env exit-stacksize exit-body)
         (bytecode-POP exit-arity)
         (assert (equal? exit-arity (- exit-stacksize stacksize)))
         (bytecode-BRANCH-to endlab)
       )) exit-infos exit-bodies)
    (bytecode-emit-label endlab)
  ))

(define (compute-exit-info env stacksize exit-label exit-vars)
  (let* (
    (arity
     (length exit-vars))
    (exit-env
     (fold (lambda (var i env)
             (stack-var env var (+ stacksize i))
       ) env exit-vars (range 0 arity)))
    (exit-stacksize (+ stacksize arity))
    )
    (make-exit-info exit-label arity exit-env exit-stacksize)))

(define (compile-exit env stacksize exit args)
  (match-let*
     ((($ <exit-info> exit-label arity exit-env exit-stacksize) (compenv-get-exit env exit))
      ; this exit has a reserved "argument space" of the stack of size arity,
      ; placed immediately *before* exit-stacksize, to which the exit arguments
      ; must be assigned.
      (argspace-start (- exit-stacksize arity)))
  (assert (equal? arity (length args)))
  ; fill the argument space
  ; (this should go first, before we change the traps or stack, as it runs arbitrary code)
  (for-each (lambda (arg i)
       (compile-expr env stacksize arg)
       (bytecode-put-u32-le ASSIGN)
       (bytecode-put-u32-le (stack-relative stacksize (+ argspace-start i)))
     ) args (range 0 arity))
  ; pop the stack, including trap handlers
  (let* ((exit-traps (compenv-get-traps exit-env)))
  (let walk-the-stack ((stacksize stacksize) (traps (compenv-get-traps env)))
     (assert (<= (length exit-traps) (length traps)))
     (if (< (length exit-traps) (length traps))
         (match-let ((((stacksize-before-trap stacksize-after-trap) . traps-rest) traps))
           (bytecode-POP-to stacksize stacksize-after-trap)
           (bytecode-put-u32-le POPTRAP)
           (walk-the-stack stacksize-before-trap traps-rest))
         (begin
           (assert (equal? traps exit-traps))
           (bytecode-POP-to stacksize exit-stacksize))))
  ; and jump!
  (bytecode-BRANCH-to exit-label)
)))

(define (vhash-map proc l)
  (alist->vhash
   (map (match-lambda ((k . v) (cons k (proc k v))))
        (vlist->list l))))
(define (vhash-filter-map proc l)
  (alist->vhash
   (filter-map (match-lambda ((k . v) (let ((r (proc k v))) (if r (cons k r) r))))
               (vlist->list l))))
(define (vhash-replace key value l) (vhash-cons key value (vhash-delete key l)))

(define vset-empty (alist->vhash #nil))
(define (vset-singleton x) (alist->vhash (cons (cons x #t) #nil)))
(define (vset-mem x s) (pair? (vhash-assoc x s)))
(define (vset-add x s) (vhash-replace x #t s))
(define (vset-union s1 s2) (vhash-fold (lambda (k v s) (vhash-replace k v s)) s1 s2))
(define (vset-difference s1 s2) (vhash-fold (lambda (k v s) (vhash-delete k s)) s1 s2))
(define (vset-list-union l) (fold vset-union vset-empty l))

(define (fv-expr expr bv)
  (match expr
    (('LVar v)
     (if (vset-mem v bv)
         vset-empty
         (vset-singleton v)))
    (('LGlobal id)
     vset-empty)
    (('LConst n)
     vset-empty)
    (('LBlock c args)
     (fv-expr-list args bv))
    (('LGetfield e f)
     (fv-expr e bv))
    (('LSetfield e1 e2)
     (vset-union (fv-expr e1 bv) (fv-expr e2 bv)))
    ((or ('LApply e es) ('LTailApply e es))
     (vset-union
      (fv-expr e bv)
      (fv-expr-list es bv)))
    (('LIf e1 e2 e3)
     (vset-union (fv-expr e1 bv)
     (vset-union (fv-expr e2 bv)
                 (fv-expr e3 bv))))
    (('LChain e1 e2)
     (vset-union (fv-expr e1 bv) (fv-expr e2 bv)))
    (('LSwitch e sw)
       (vset-union
        (fv-expr e bv)
        (fv-switch sw bv)))
    (('LCatch e1 v e2)
       (vset-union
        (fv-expr e1 bv)
        (fv-expr e2 (bv-add-var v bv))))
    (('LReraise e)
     (fv-expr e bv))
    (('LLet v e body)
     (vset-union
      (fv-expr e bv)
      (fv-expr body (bv-add-var v bv))))
    (('LLetfun f args fun body)
     (vset-union
      (fv-expr fun (bv-add-vars args bv))
      (fv-expr body (bv-add-var f bv))))
    (('LLetrecfun bindings body)
     (let* ((vars (map car bindings))
            (funs (map cadddr bindings))
            (rec-bv (bv-add-vars vars bv)))
       (vset-union
        (fv-expr-list funs rec-bv)
        (fv-expr body rec-bv))))
    (('LLetexits defs body)
     ; exit names live in a separate namespace, they are not (free) variables
     (vset-union
      (vset-list-union (map (match-lambda ((exit-name exit-vars exit-body)
           (fv-expr exit-body (bv-add-vars exit-vars bv))
         )) defs))
      (fv-expr body bv)))
    (('LExit exit args)
     (fv-expr-list args bv))
  ))

(define (fv-expr-list exprs bv)
  (vset-list-union
   (map (lambda (e) (fv-expr e bv)) exprs)))

(define (fv-switch sw bv)
  (match-let ((($ <switch> consts blocks default nums) sw))
    (vset-list-union (append
      (map (match-lambda
            (($ <switch-const> tag rhs)
             (fv-expr rhs bv))
         ) consts)
      (map (match-lambda
            (($ <switch-block> tag arity vars rhs)
             (fv-expr rhs (bv-add-vars vars bv)))
        ) blocks)
      (list ((match-lambda
              (#nil vset-empty)
              ((var . rhs) (fv-expr rhs (bv-add-var var bv)))
        ) default))
  ))))

(define bv-empty vset-empty)

(define (bv-add-vars vars bv)
  (fold bv-add-var bv vars))

(define (bv-add-var var bv)
  (if (null? var)
    bv
    (vset-add var bv)))

(define (range a b) (if (>= a b) #nil (cons a (range (+ a 1) b))))

(define (compile-fundef-body env args body nfv lab recvars recoffset)
  (let* ((arity (length args))
         (envoff (* rec-closure-step (- (- (length recvars) 1) recoffset)))
         (mvars (vhash-filter-map
                 (lambda (name v)
                   (let ((r (vhash-assoc name nfv)))
                     (if (pair? r)
                         (list 'VarEnv (+ envoff (cdr r)))
                         #f)))
                 (compenv-get-vars env)))
         (rvars (fold (lambda (rec-name i vs)
                        (vhash-replace
                         rec-name
                         (list 'VarRec (* rec-closure-step (- i recoffset)))
                         vs))
                      mvars recvars (range 0 (length recvars))))
         (nvars (fold (lambda (arg i vs)
                        (vhash-replace
                         arg
                         (list 'VarStack (- (- arity 1) i))
                         vs))
                      rvars args (range 0 arity)))
         (nenv (compenv-with-vars compenv-empty nvars))
         )
    (assert (> arity 0))
    (bytecode-put-u32-le RESTART)
    (bytecode-emit-label lab)
    (bytecode-put-u32-le GRAB)
    (bytecode-put-u32-le (- arity 1))
    (compile-expr nenv arity body)
    (bytecode-put-u32-le RETURN)
    (bytecode-put-u32-le arity)
  ))

(define (fv-list fv) (map car (vlist->list fv)))
(define (make-nfv fv) (fold (lambda (name i nfv) (vhash-replace name i nfv)) vlist-null fv (range 0 (length fv))))

(define (compile-fundef env stacksize args body)
  (let* ((fv (fv-list (fv-expr body (bv-add-vars args bv-empty))))
         (nfv (make-nfv fv))
         (lab1 (newlabel))
         (lab2 (newlabel))
         )
    (compile-args env stacksize (map lid->lvar fv))
    (bytecode-BRANCH-to lab1)
    (compile-fundef-body env args body nfv lab2 #nil -1)
    (bytecode-emit-label lab1)
    (bytecode-put-u32-le CLOSURE)
    (bytecode-put-u32-le (length fv))
    (bytecode-emit-labref lab2)
    ))

(define (compile-recfundefs env stacksize funs)
  (let* ((numfuns (length funs))
         (names  (map car funs))
         (argss  (map cadr funs))
         (bodies (map caddr funs))
         (fvenv (bv-add-vars names bv-empty))
         (fv (fv-list (vset-list-union
                (map (lambda (body args) (fv-expr body (bv-add-vars args fvenv))) bodies argss))))
         (nfv (make-nfv fv))
         (labs (map (lambda (_) (newlabel)) funs))
         (endlab (newlabel))
         (nenv (fold (lambda (name pos env) (stack-var env name (+ stacksize pos)))
                     env names (range 0 numfuns)))
         )
    (compile-args env stacksize (map lid->lvar fv))
    (bytecode-BRANCH-to endlab)
    (for-each (lambda (args body lab pos)
                (compile-fundef-body env args body nfv lab names pos))
              argss bodies labs (range 0 numfuns))
    (bytecode-emit-label endlab)
    (bytecode-put-u32-le CLOSUREREC)
    (bytecode-put-u32-le numfuns)
    (bytecode-put-u32-le (length fv))
    (let* ((basepos (bytecode-get-pos)))
      (for-each (lambda (lab) (bytecode-emit-labref-with-pos lab basepos)) labs))
    nenv
  ))

(define empty-numtags (cons 0 0))
(define (numtags-next arity numtags)
  (match-let (((const-count . block-count) numtags))
   (if (> arity 0)
       (cons block-count (cons const-count (+ 1 block-count)))
       (cons const-count (cons (+ 1 const-count) block-count)))))

(define (expand-constr-def env type-name constr-def)
  (match constr-def
    ((constr-name . ('VariantTuple arity))
     (cons env (cons constr-name arity)))
    ((constr-name . ('VariantRecord record-def))
     ; inline records are expanded naively:
     ;     type t = A of { x : int }
     ; becomes
     ;     type t = A of t#A
     ;     and t#A = { x : int }
     ; (type parameters are ignored, as for other type declarations)
     ;
     ; Note: this representation is incompatible with OCaml, so FFI or mixed compilation
     ; across interfaces that use inline records would not work.
    (cons
     (compile-type env (string-append type-name "#" constr-name) (list 'IRecord record-def))
     (cons constr-name 1)))
  ))

(define (compile-type env type-name tdef)
  (match tdef
   (('ISum constrs)
    (match-let* (
           ((env . constrs)
            (fold-right (lambda (constr-def acc)
                (match-let* (
                  ((env . expanded-constrs) acc)
                  ((env . expanded-constr) (expand-constr-def env type-name constr-def))
                 ) (cons env (cons expanded-constr expanded-constrs)))
              ) (cons env #nil) constrs))
           (final-numtags
            (fold (match-lambda* (((name . arity) cur-numtags)
                    (match-let (((next-tag . next-numtags) (numtags-next arity cur-numtags)))
                      next-numtags)))
                  empty-numtags constrs))
           (nenv-constrs
            (car (fold (match-lambda* (((name . arity) (e . cur-numtags))
                         (match-let (((next-tag . next-numtags) (numtags-next arity cur-numtags)))
                           (cons
                            (bindings-replace name (mkconstr arity next-tag final-numtags) e)
                            next-numtags))))
                       (cons (env-get-constrs env) empty-numtags) constrs)))
      )
      ; (newline)(display type-name)(newline)(display numtags)(newline)(newline)
      (env-with-constrs env nenv-constrs)))
    (('IRecord l)
         (let* ((numfields (length l))
                (nenv-fields (car (fold (match-lambda* ((name (e . i))
                                          (cons (bindings-replace name (mkfield i numfields) e) (+ 1 i))))
                                        (cons (env-get-fields env) 0) l))))
           (env-with-fields env nenv-fields)))
    (('IRebind)
     env)))

(define exnid 0)
(define (declare-exn name arity env)
  (set! exnid (+ 1 exnid))
  (env-replace-constr env name (mkconstr arity exnid (cons -2 -2))))

(define (env-open env mod)
  (env-merge env (env-make-local (env-only-exported (module-get-env mod)))))

(define (apply-functor mod arg)
  (match mod
   (('VFunctor env argname body)
    (compile-module (env-replace-module env argname arg) body))))

(define (compile-module env m)
  (match m
   (('MEStruct defs) (list 'VModule (compile-defs (env-make-local env) defs)))
   (('MEFunctor argname body) (list 'VFunctor env argname body))
   (('MEApply name args)
    (let* ((mod (env-get-module env name))
           (margs (map (lambda (m) (compile-module env m)) args)))
      (fold (lambda (arg m) (apply-functor m arg)) mod margs))
    )))

(define (compile-def env d)
  (match d
   (('MOpen m)
    (let* ((menv (env-get-module env m)))
      (env-open env menv)))
   (('MException name constr-info)
    (match-let* (
     ((env . (name . arity)) (expand-constr-def env "exn" (cons name constr-info)))
    ) (declare-exn name arity env)))
   (('MLet rec-flag bindings)
    (let* ((locations (map (lambda (def) (if (equal? (def-get-name def) "_") #nil (slot-for-global))) bindings))
           (nenv-vars (fold (match-lambda* ((($ <def> name args body) loc e)
                             (let ((shape (map arg-get-label args)))
                               (if (equal? name "_") e
                                   (bindings-replace
                                      name (mkvar (list 'VarGlobal loc) shape)
                                      e))))
                         ) (env-get-vars env) bindings locations))
           (nenv (env-with-vars env nenv-vars))
           (tenv (if rec-flag nenv env)))
      (for-each (match-lambda* ((($ <def> name args body) loc)
                    ; (display "Compiling ") (display name) (newline)
                    (if (null? args)
                        (compile-high-expr tenv #f body)
                        (match-let* (((args shape body) (lower-function tenv args body)))
                          (compile-fundef compenv-empty 0 args body)))
                    (if (not (null? loc))
                        (begin (bytecode-put-u32-le SETGLOBAL)
                               (bytecode-put-u32-le loc)))
                  )) bindings locations)
      nenv
      ))
   (('MTypedef tdefs)
    (fold (lambda (tdef env) (compile-type env (car tdef) (cdr tdef))) env tdefs))
   (('MModule name mod)
    (env-replace-module env name (compile-module env mod)))
   (('MModuleType name ())
    env)
   (('MExternal name arity primname)
    (match-let* ((shape (make-list arity (list 'Nolabel)))
           ((prim-kind . prim-num) (prim primname))
           (lab1 (newlabel))
           (lab2 (newlabel))
           (pos (slot-for-global)))
      (assert (> arity 0))
      (bytecode-BRANCH-to lab1)
      (bytecode-put-u32-le RESTART)
      (bytecode-emit-label lab2)
      (bytecode-put-u32-le GRAB)
      (bytecode-put-u32-le (- arity 1))
      (do ((i 0 (1+ i))) ((>= i (- arity 1)))
        (begin
          (bytecode-ACC (- arity 1))
          (bytecode-put-u32-le PUSH)))
      (bytecode-ACC (- arity 1))
      (match prim-kind
        ('Internal #f)
        ('C
          (cond ((= arity 1) (bytecode-put-u32-le C_CALL1))
                ((= arity 2) (bytecode-put-u32-le C_CALL2))
                ((= arity 3) (bytecode-put-u32-le C_CALL3))
                ((= arity 4) (bytecode-put-u32-le C_CALL4))
                ((= arity 5) (bytecode-put-u32-le C_CALL5))
                (else (begin
                        (bytecode-put-u32-le C_CALLN)
                        (bytecode-put-u32-le arity))))))
      (bytecode-put-u32-le prim-num)
      (bytecode-put-u32-le RETURN)
      (bytecode-put-u32-le arity)
      (bytecode-emit-label lab1)
      (bytecode-put-u32-le CLOSURE)
      (bytecode-put-u32-le 0)
      (bytecode-emit-labref lab2)
      (bytecode-put-u32-le SETGLOBAL)
      (bytecode-put-u32-le pos)
      (env-replace-var env name (mkvar (list 'VarGlobal pos) shape))
      ))
   ))

(define (compile-defs env defs)
  (match defs
   (#nil
    env)
   ((def . rest)
    (compile-defs (compile-def env def) rest))))

(define initial-env
  (env-replace-constr empty-env "" (mkconstr -1 0 (cons 0 1))))

(define (declare-builtin-exn name arity)
  (set! initial-env (declare-exn name arity initial-env))
  (newglob exnid))

; Order sensitive! Must match declarations in runtime/caml/fail.h
; Do NOT call newglob before this point: the builtin exceptions must correspond to the globals 0-11
(declare-builtin-exn "Out_of_memory" 0)
(declare-builtin-exn "Sys_error" 1)
(declare-builtin-exn "Failure" 1)
(declare-builtin-exn "Invalid_argument" 1)
(declare-builtin-exn "End_of_file" 0)
(declare-builtin-exn "Division_by_zero" 0)
(declare-builtin-exn "Not_found" 0)
(declare-builtin-exn "Match_failure" 1)
(declare-builtin-exn "Stack_overflow" 0)
(declare-builtin-exn "Sys_blocked_io" 0)
(declare-builtin-exn "Assert_failure" 1)
(declare-builtin-exn "Undefined_recursive_module" 1)

(define (parse-input-file file)
  (call-with-input-file file (lambda (port)
    (set-current-input-port port)
    ((ml-parser) (lambda () (token errorp)) errorp))))

(define (input-file->module-name file)
  (string-capitalize (basename file ".ml")))

(define (parse-phrases input-phrases)
  (map (match-lambda
     (('Module modname file)
       (list 'MModule modname (list 'MEStruct (parse-input-file file))))
     (('Open modname)
       (list 'MOpen (list 'Lident modname)))
     ) input-phrases))

(define input-phrases-q (make-q))
(define output-file "out.byte")
(define (usage-and-exit)
  (display "Usage: guile compile.scm input.ml -o output\n")
  (exit))

(define (process-args args)
  (match args
         (#nil '())
         (("-h" . rest) (usage-and-exit))
         (("-o" outfile . rest)
          (set! output-file outfile)
          (process-args rest))
         (("--open" modname . rest)
          (enq! input-phrases-q (list 'Open modname))
          (process-args rest))
         ((infile . rest)
          (enq! input-phrases-q (list 'Module (input-file->module-name infile) infile))
          (process-args rest))
  ))
(if (null? (cdr (program-arguments))) (usage-and-exit))
(process-args (cdr (program-arguments)))

(define (queue->list q)
  (unfold q-empty? deq! (lambda (q) q) q))
(define prog (parse-phrases (queue->list input-phrases-q)))

(bytecode-open-output output-file)

(bytecode-begin-section "CODE")
(compile-defs initial-env prog)
(bytecode-put-u32-le STOP)

(bytecode-begin-section "PRIM")
(bytecode-write-prims)

(bytecode-begin-section "DATA")
(bytecode-write-globals)

(bytecode-close-output)
