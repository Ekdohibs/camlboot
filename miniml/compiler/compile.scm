(use-modules (system base lalr)
             (srfi srfi-1) (srfi srfi-9 gnu)
             (rnrs base)
             (ice-9 q)
             (ice-9 binary-ports) (ice-9 vlist) (ice-9 match))


(define rec-closure-step 2)
(define has-custom-fixed #f)
(define magic "Caml1999X023")

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

(define (mkapp fname args) (list 'EApply (lid->evar fname) (map mknolabelapp args)))
(define (mkapp1 fname arg1) (mkapp fname (list arg1)))
(define (mkapp2 fname arg1 arg2) (mkapp fname (list arg1 arg2)))
(define (mkapp3 fname arg1 arg2 arg3) (mkapp fname (list arg1 arg2 arg3)))

(define (mkuminus arg)
  (match arg
         (('EConstant ('CInt (n . c))) (list 'EConstant (list 'CInt (cons (- n) c))))
         (_ (mkapp1 "~-" arg))))

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

; Dummy
(define (mkintervalpat n1 n2)
  (if (= n1 n2) (list 'PInt n1) (list 'POr (list 'PInt n1) (mkintervalpat (+ n1 1) n2))))

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
           AND BEGIN DO DONE DOTDOT DOWNTO END EXCEPTION EXTERNAL FOR FUN FUNCTION FUNCTOR IF IN MODULE
           MUTABLE OF OPEN REC SIG STRUCT TO TRY TYPE VAL WHEN WHILE WITH
           EOF STRING LIDENT UIDENT INT
           (right: MINUSGT)
           (left: AS)
           (left: BAR)
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
     (MODULE UIDENT functor_args COLON module_type EQ module_expr) : (list 'MModule $2 (mkfunctor $3 $7))
     (MODULE TYPE UIDENT EQ module_type) : (list 'MModuleType $3 $5)
     (EXTERNAL lident_ext COLON type_count_arrows EQ STRING) : (list 'MExternal $2 $4 $6)
     (EXTERNAL lident_ext COLON type_count_arrows EQ STRING STRING) : (list 'MExternal $2 $4 $6))

   (module_expr
    (STRUCT list_semidefinition END) : (list 'MEStruct $2)
    (FUNCTOR functor_args MINUSGT module_expr) : (mkfunctor $2 $4)
    (longident_uident functor_apply) : (list 'MEApply $1 $2))

   (functor_apply
    ( ) : #nil
    (LPAREN module_expr RPAREN functor_apply) : (cons $2 $4))

   (signature_item
    (TYPE typedef type_ands) : '()
    (EXCEPTION constr_decl) : '()
    (VAL lident_ext COLON type_ignore) : '())

   (signature
    ( ) : '()
    (SEMICOLONSEMICOLON signature) : '()
    (signature_item signature) : '())

   (module_type
    (longident_uident) : '()
    (SIG signature END) : '()
    (FUNCTOR functor_args MINUSGT module_type) : '())

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
    (type_name_with_args EQ type_ignore EQ separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $5))
    (type_name_with_args EQ type_ignore EQ BAR separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $6))
    (type_name_with_args EQ type_ignore EQ record_def) : (cons $1 (list 'IRecord $5))
    (type_name_with_args EQ type_ignore) : (cons $1 (list 'IRebind)))

   (record_def
    (LBRACE separated_semi_opt_field_decl RBRACE) : $2)

   (let_ands
    ( ) : #nil
    (AND letdef let_ands) : (cons $2 $3))

   (letdef
    (LPAREN RPAREN EQ expr) : (mkdef "_" #nil $4)
    (lident_ext list_labelled_arg EQ expr) : (mkdef $1 $2 $4)
    (lident_ext list_labelled_arg COLON type_ignore EQ expr) : (mkdef $1 $2 $6))

   (list_labelled_arg
    ( ) : #nil
    (nonempty_list_labelled_arg) : $1)

   (nonempty_list_labelled_arg
    (labelled_arg) : (list $1)
    (labelled_arg nonempty_list_labelled_arg) : (cons $1 $2))

   (labelled_arg
    (simple_pattern) : (mknolabelfun $1)
    (TILDE LIDENT) : (mkarg (list 'PVar $2) (list 'Labelled $2) (list 'None))
    (TILDE LIDENT COLON simple_pattern) : (mkarg $4 (list 'Labelled $2) (list 'None))
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
    (QUESTION type_ignore) : '()
    (TILDE type_ignore) : '()
    (COLON type_ignore) : '()
    (LPAREN type_ignore RPAREN type_ignore) : '())

   (type_count_stars
    ( ) : 0
    (STAR type_count_stars) : (+ 1 $2)
    (longident_field type_count_stars) : $2
    (QUOTE type_count_stars) : $2
    (LPAREN type_ignore RPAREN type_count_stars) : $4)

   (type_count_arrows
    ( ) : 0
    (QUESTION type_count_arrows) : $2
    (TILDE type_count_arrows) : $2
    (COLON type_count_arrows) : $2
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
    (record_item_pattern) : (if (equal? (car $1) (list 'Lident "_")) #nil (cons $1 #nil))
    (record_list_pattern SEMICOLON record_item_pattern) : (if (equal? (car $3) (list 'Lident "_")) $1 (cons $3 $1)))

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
    (longident_uident DOT LBRACE record_list_pattern option_semicolon RBRACE) :
        (list 'POpen $1 (list 'PRecord (reverse $4)))
    (longident_uident DOT LPAREN pattern RPAREN) : (list 'POpen $1 $4)
    (INT) : (if (null? (cdr $1)) (list 'PInt (car $1))
                (errorp "Integer literals with non-empty extension unsupported in patterns"))
    (INT DOTDOT INT) :
        (if (and (null? (cdr $1)) (null? (cdr $3))) (mkintervalpat (car $1) (car $3))
            (errorp "Integer literals with non-empty extension unsupported in patterns"))
    (STRING) : (list 'PString $1)
   )

   (simple_expr
    (longident_lident) : (list 'EVar $1)
    (constant) : (list 'EConstant $1)
    (longident_constr (prec: dot_prec)) : (list 'EConstr $1 #nil)
    (LPAREN expr RPAREN) : $2
    (BEGIN expr END) : $2
    (LPAREN expr COLON type_ignore RPAREN) : $2
    (simple_expr DOT longident_field) : (list 'EGetfield $1 $3)
    (longident_uident DOT LBRACE record_list_expr option_semicolon RBRACE) :
        (list 'ELetOpen $1 (list 'ERecord (reverse $4)))
    (longident_uident DOT LBRACE simple_expr WITH record_list_expr option_semicolon RBRACE) :
        (list 'ELetOpen $1 (list 'ERecordwith $4 (reverse $6)))
    (LBRACE record_list_expr option_semicolon RBRACE) : (list 'ERecord (reverse $2))
    (LBRACE simple_expr WITH record_list_expr option_semicolon RBRACE) : (list 'ERecordwith $2 (reverse $4))
    (LBRACK RBRACK) : (lid->econstr "[]" #nil)
    (LBRACK semi_separated_expr_list_opt RBRACK) :
        (fold-right (lambda (e r) (lid->econstr "::" (list e r))) (lid->econstr "[]" #nil) $2)
    (LBRACKBAR BARRBRACK) : (list 'EVar (list 'Lident "__atom0"))
    (LBRACKBAR semi_separated_expr_list_opt BARRBRACK) : (lid->econstr "" $2)
    (PREFIXOP simple_expr) : (mkapp1 $1 $2)
    (simple_expr DOT LPAREN expr RPAREN) : (mkapp2 "__array_get" $1 $4)
    (simple_expr DOT LBRACK expr RBRACK) : (mkapp2 "__string_get" $1 $4)
    (longident_uident DOT LPAREN expr RPAREN) : (list 'ELetOpen $1 $4)
    (WHILE expr DO expr DONE) : (list 'EWhile $2 $4)
    (FOR lident_ext EQ expr TO expr DO expr DONE) : (list 'EFor $2 'UpTo $4 $6 $8)
    (FOR lident_ext EQ expr DOWNTO expr DO expr DONE) : (list 'EFor $2 'DownTo $4 $6 $8))

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

   (expr_no_semi1
    (simple_expr) : $1
    (MINUS expr_no_semi1 (prec: uminus_prec)) : (mkuminus $2)
    (simple_expr nonempty_list_labelled_simple_expr) : (list 'EApply $1 $2)
    (longident_constr simple_expr) : (list 'EConstr $1 (cons $2 #nil)))

   (expr_no_semi
    (expr_no_semi1) : $1
    (FUN nonempty_list_labelled_arg MINUSGT expr) : (mklambda $2 $4)
    (comma_separated_list2_expr (prec: comma_prec)) : (list 'EConstr (list 'Lident "") (reverse $1))
    (simple_expr DOT longident_field LTMINUS expr_no_semi) : (list 'ESetfield $1 $3 $5)
    (IF expr THEN expr_no_semi ELSE expr_no_semi) : (list 'EIf $2 $4 $6)
    (IF expr THEN expr_no_semi) : (list 'EIf $2 $4 (list 'EConstant (list 'CUnit)))
    (expr_no_semi INFIXOP0 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi INFIXOP1 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi INFIXOP2 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi INFIXOP3 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi INFIXOP4 expr_no_semi) : (mkapp2 $2 $1 $3)
    (expr_no_semi EQ expr_no_semi) : (mkapp2 "=" $1 $3)
    (expr_no_semi PLUS expr_no_semi) : (mkapp2 "+" $1 $3)
    (expr_no_semi MINUS expr_no_semi) : (mkapp2 "-" $1 $3)
    (expr_no_semi PERCENT expr_no_semi) : (mkapp2 "%" $1 $3)
    (expr_no_semi STAR expr_no_semi) : (mkapp2 "*" $1 $3)
    (expr_no_semi COLONEQ expr_no_semi) : (mkapp2 ":=" $1 $3)
    (expr_no_semi AMPERAMPER expr_no_semi) : (list 'EIf $1 $3 (list 'EConstant (list 'CInt (cons 0 #nil))))
    (expr_no_semi BARBAR expr_no_semi) : (list 'EIf $1 (list 'EConstant (list 'CInt (cons 1 #nil))) $3)
    (expr_no_semi BARGT simple_expr list_labelled_simple_expr):
      ;; (e |> f e1 e2 .. en) ~> f e1 .. en e
      (list 'EApply $3 (append $4 (list (mknolabelapp $1))))
    (simple_expr list_labelled_simple_expr ATAT expr_no_semi):
      ;; (f e1 .. en @@ e) ~> f e1 .. en e
      (list 'EApply $1 (append $2 (list (mknolabelapp $4))))
    (MATCH expr WITH clauses) : (list 'EMatch $2 $4)
    (TRY expr WITH clauses) :
      (match-let* ((clauses $4)
                   ((value-clauses exn-clauses) (split-clauses clauses)))
       (if (not (null? exn-clauses)) (errorp "exception clauses are invalid with 'try'"))
       (list 'ETry $2 clauses))
    (FUNCTION clauses) :
      (match-let* ((clauses $2)
                   ((value-clauses exn-clauses) (split-clauses clauses)))
       (if (not (null? exn-clauses)) (errorp "exception clauses are invalid with 'function'"))
       (mklambda
        (list (mknolabelfun (lid->pvar "arg#function")))
        (list 'EMatch (lid->evar "arg#function") clauses)))
    (LET llet llet_ands IN expr (prec: LET)) : (list 'ELet #f (cons $2 $3) $5)
    (LET REC llet llet_ands IN expr (prec: LET)) : (list 'ELet #t (cons $3 $4) $6)
    (LET OPEN longident_uident IN expr (prec: LET)) : (list 'ELetOpen $3 $5)
    (expr_no_semi COLONCOLON expr_no_semi) : (lid->econstr "::" (cons $1 (cons $3 #nil)))
    (simple_expr DOT LPAREN expr RPAREN LTMINUS expr_no_semi) : (mkapp3 "__array_set" $1 $4 $7)
    (simple_expr DOT LBRACK expr RBRACK LTMINUS expr_no_semi) : (mkapp3 "__string_set" $1 $4 $7)
    (LET percent_exit llet llet_ands IN expr (prec: LET)) : (list 'ELetExits (cons $3 $4) $6)
    (LBRACK percent_exit RBRACK LIDENT list_labelled_simple_expr) : (list 'EExit $4 $5)
    )

   (percent_exit
    (PERCENT LIDENT) : (if (equal? $2 "exit") #nil
                           (errorp "expected the %exit extension")))

   (expr
    (expr_no_semi) : $1
    (expr_no_semi SEMICOLON) : $1
    (expr_no_semi SEMICOLON expr) : (list 'EChain $1 $3))

   (llet
    (pattern EQ expr) : (cons $1 $3)
    (lident_ext nonempty_list_labelled_arg EQ expr) : (cons (list 'PVar $1) (mklambda $2 $4))
    (lident_ext nonempty_list_labelled_arg COLON type_ignore EQ expr) : (cons (list 'PVar $1) (mklambda $2 $6)))

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
    (value-clause) : (cons 'ClValue $1)
    (exception-clause) : (cons 'ClException $1)
   )

   (value-clause
    (pattern clause_rhs) : (list $1 (car $2) (cadr $2))
   )

   (exception-clause
    (EXCEPTION pattern clause_rhs) : (list $2 (car $3) (cadr $3))
    )

   (clause_rhs
    (MINUSGT expr) : (list #nil $2)
    (WHEN expr MINUSGT expr) : (list $2 $4)
   )

   (field_decl
    (LIDENT COLON type_ignore) : $1
    (MUTABLE LIDENT COLON type_ignore) : $2)


 ))

(define kw (list
    (cons "and" (cons 'AND #f))
    (cons "as" (cons 'AS #f))
    (cons "asr" (cons 'INFIXOP4 "asr"))
    (cons "begin" (cons 'BEGIN #f))
    (cons "do" (cons 'DO #f))
    (cons "done" (cons 'DONE #f))
    (cons "downto" (cons 'DOWNTO #f))
    (cons "else" (cons 'ELSE #f))
    (cons "end" (cons 'END #f))
    (cons "exception" (cons 'EXCEPTION #f))
    (cons "external" (cons 'EXTERNAL #f))
    (cons "false" (cons 'UIDENT "false"))
    (cons "for" (cons 'FOR #f))
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
    (cons "to" (cons 'TO #f))
    (cons "true" (cons 'UIDENT "true"))
    (cons "try" (cons 'TRY #f))
    (cons "type" (cons 'TYPE #f))
    (cons "val" (cons 'VAL #f))
    (cons "when" (cons 'WHEN #f))
    (cons "while" (cons 'WHILE #f))
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
  (let loop ((acc #nil))
    (let* ((c (read-char)))
      (cond ((eof-object? c) (errorp "Unterminated string"))
            ((char=? c #\") (reverse acc))
            ((char=? c #\\ )
             (if (char=? (peek-char) #\newline)
                 (begin (read-char) (while (space-or-tab? (peek-char)) (read-char)) (loop acc))
                 (loop (cons (escape-sequence errorp) acc))))
            (else (loop (cons c acc)))
  ))))

(define (char-alphanumeric? c) (or (char-alphabetic? c) (char-numeric? c)))

(define (ident errorp)
  (let ((c (peek-char)))
        (cond ((eof-object? c) #nil)
              ((or (char-alphanumeric? c) (or (char=? c #\_) (char=? c #\'))) (begin (read-char) (cons c (ident errorp))))
              (else #nil)
              )))

(define (char->digit c) (- (char->integer c) (char->integer #\0)))

(define (number-chars errorp)
  (let ((c (peek-char)))
    (cond ((eof-object? c) #nil)
          ((char=? c #\_) (begin (read-char) (number-chars errorp)))
          ((char-numeric? c) (begin (read-char) (cons (char->digit c) (number-chars errorp))))
          (else #nil)
          )))

(define (hex-chars errorp)
  (let ((c (peek-char)))
    (cond ((eof-object? c) #nil)
          ((char=? c #\_) (begin (read-char) (hex-chars errorp)))
          ((char-numeric? c)
           (begin
             (read-char)
             (cons (char->digit c) (hex-chars errorp))))
          ((and (char<=? #\a c) (char<=? c #\f))
           (begin
             (read-char)
             (cons (+ 10 (- (char->integer c) (char->integer #\a))) (hex-chars errorp))))
          ((and (char<=? #\A c) (char<=? c #\F))
           (begin
             (read-char)
             (cons (+ 10 (- (char->integer c) (char->integer #\A))) (hex-chars errorp))))
          (else #nil)
          )))

(define (list->number l base)
  (fold (lambda (d acc) (assert (and (<= 0 d) (< d base))) (+ d (* base acc))) 0 l))

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

(define (mkint location n)
  (let ((c (peek-char)))
    (if (or (char-lower-case? c) (char-upper-case? c))
        (begin
          (read-char)
          (make-lexical-token 'INT location (cons n c)))
        (make-lexical-token 'INT location (cons n #nil)))
    ))

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
                            (if (char=? (peek-char) #\@)
                                (begin (do () ((char=? (read-char) #\]))) (token errorp))
                                (make-lexical-token 'LBRACK location #f))))
        ((char=? c #\]) (make-lexical-token 'RBRACK location #f))
        ((char=? c #\;) (if (char=? (peek-char) #\;)
                            (begin (read-char) (make-lexical-token 'SEMICOLONSEMICOLON location #f))
                            (make-lexical-token 'SEMICOLON location #f)))
        ((char=? c #\.) (if (char=? (peek-char) #\.)
                            (begin (read-char) (make-lexical-token 'DOTDOT location #f))
                            (make-lexical-token 'DOT location #f)))
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
        ; All other characters that can begin an operator
        ((string-index "+-=*~@^?!&<>/%$" c) (mksymbol location c))
        ((char=? c #\") (make-lexical-token 'STRING location (list->string (string-chars errorp))))
        ((char=? c #\') (let ((c (read-char)))
                             (if (char=? c #\\ )
                                 (let* ((nc (escape-sequence errorp))
                                        (c2 (read-char)))
                                   (if (char=? c2 #\')
                                       (make-lexical-token 'INT location (cons (char->integer nc) #nil))
                                       (errorp "Unterminated character literal")
                                   ))
                                 (if (char=? (peek-char) #\')
                                     (begin
                                       (read-char) (make-lexical-token 'INT location (cons (char->integer c) #nil)))
                                     (begin (unread-char c) (make-lexical-token 'QUOTE location #f))))
                             ))
        ((or (char-lower-case? c) (char=? c #\_)) (mktoken location (get-lident (list->string (cons c (ident errorp))))))
        ((char-upper-case? c) (make-lexical-token 'UIDENT location (list->string (cons c (ident errorp)))))
        ((char-numeric? c) (cond
                            ((and (char=? c #\0) (char=? (peek-char) #\x))
                             (read-char)
                             (mkint location (list->number (hex-chars errorp) 16))
                             )
                            ((and (char=? c #\0) (char=? (peek-char) #\o))
                             (read-char)
                             (mkint location (list->number (number-chars errorp) 8))
                             )
                            ((and (char=? c #\0) (char=? (peek-char) #\b))
                             (read-char)
                             (mkint location (list->number (number-chars errorp) 2))
                             )
                            (else (mkint location (list->number (cons (char->digit c) (number-chars errorp)) 10)))))
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
    (put-string bytecode-output-port magic)
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
          (match-lambda
           (('Integer n)
              (bytecode-put-u8 #x3)
              (bytecode-put-u64 n)
              (set! len (+ len 9)))
           (('Int64 n)
              (bytecode-put-u8 (if has-custom-fixed #x19 #x12))
              (bytecode-put-string "_j")
              (bytecode-put-u8 0)
              (bytecode-put-u64 n)
              (set! len (+ len 12))
              (set! size64 (+ size64 3)))
           (('Int32 n)
              (bytecode-put-u8 (if has-custom-fixed #x19 #x12))
              (bytecode-put-string "_i")
              (bytecode-put-u8 0)
              (bytecode-put-u32 n)
              (set! len (+ len 8))
              (set! size64 (+ size64 3)))
           (('Intnat n)
              (bytecode-put-u8 (if has-custom-fixed #x19 #x12))
              (bytecode-put-string "_n")
              (bytecode-put-u8 0)
              (bytecode-put-u8 2)
              (bytecode-put-u64 n)
              (set! len (+ len 13))
              (set! size64 (+ size64 3)))
           (('String s)
              (bytecode-put-u8 #x15)
              (bytecode-put-u64 (string-length s))
              (bytecode-put-string s)
              (set! len (+ len (+ 9 (string-length s))))
              (set! size64 (+ size64 (+ 1 (ash (+ (string-length s) 8) -3)))))
           (('Block tag values)
              (let ((sz (length values)))
                (if (= sz 0)
                    (begin
                      (bytecode-put-u8 #x8)
                      (bytecode-put-u32 tag)
                      (set! len (+ len 5)))
                    (begin
                      (bytecode-put-u8 #x13)
                      (bytecode-put-u64 (+ tag (ash sz 10)))
                      (set! len (+ len 9))
                      (set! size64 (+ size64 (+ 1 sz)))
                      (for-each loop values)))))
              )))
      (loop value)
      (bytecode-backpatch-u64 lenpos len)
      (bytecode-backpatch-u64 objcountpos 0)
      (bytecode-backpatch-u64 size64pos size64)
      )))

(define globs #nil)
(define globnames #nil)
(define nglobs 0)
(define (newglob value name)
  (begin
    (set! globs (cons value globs))
    (set! globnames (cons (string-append name " (" (number->string nglobs) ")") globnames))
    (set! nglobs (+ 1 nglobs))
    (- nglobs 1)))
(define (slot-for-global name) (newglob (list 'Integer 0) name))
(define (bytecode-write-globals)
  (bytecode-marshal (list 'Block 0 (reverse globs))))

(define (mktbl names n)
  (if (null? names) (list 'Integer 0)
        (list 'Block 0 (list (mktbl (cdr names) (- n 1)) (list 'Block 0 (list (list 'Integer 0) (list 'String (car names)) (list 'Integer 0))) (list 'Integer (- n 1)) (list 'Integer 0) (list 'Integer n)))))

(define (bytecode-write-symbols)
  (bytecode-marshal (list 'Block 0 (list (list 'Integer nglobs) (mktbl globnames nglobs)))))

(define known-prims (list
  (cons "%raise" "%91")
  (cons "%reraise" "%146")
  (cons "%raise_notrace" "%147")
  (cons "%equal" "caml_equal")
  (cons "%notequal" "caml_notequal")
  (cons "%lessthan" "caml_lessthan")
  (cons "%greaterthan" "caml_greaterthan")
  (cons "%lessequal" "caml_lessequal")
  (cons "%greaterequal" "caml_greaterequal")
  (cons "%compare" "caml_compare")
  (cons "%eq" "%121")
  (cons "%noteq" "%122")
  (cons "%negint" "%109")
  (cons "%succint" "%127,1")
  (cons "%predint" "%127,-1")
  (cons "%addint" "%110")
  (cons "%subint" "%111")
  (cons "%mulint" "%112")
  (cons "%divint" "%113")
  (cons "%modint" "%114")
  (cons "%andint" "%115")
  (cons "%orint" "%116")
  (cons "%xorint" "%117")
  (cons "%lslint" "%118")
  (cons "%lsrint" "%119")
  (cons "%asrint" "%120")
  (cons "%boolnot" "%88")

  (cons "%negfloat" "caml_neg_float")
  (cons "%addfloat" "caml_add_float")
  (cons "%subfloat" "caml_sub_float")
  (cons "%mulfloat" "caml_mul_float")
  (cons "%divfloat" "caml_div_float")
  (cons "%absfloat" "caml_abs_float")
  (cons "%floatofint" "caml_float_of_int")
  (cons "%intoffloat" "caml_int_of_float")

  (cons "%bytes_length" "caml_ml_bytes_length")
  (cons "%bytes_safe_get" "caml_bytes_get")
  (cons "%bytes_unsafe_get" "%82")
  (cons "%bytes_safe_set" "caml_bytes_set")
  (cons "%bytes_unsafe_set" "%83")

  (cons "%string_length" "caml_ml_string_length")
  (cons "%string_safe_get" "caml_string_get")
  (cons "%string_unsafe_get" "%148")
  (cons "%string_safe_set" "caml_bytes_set")
  (cons "%string_unsafe_set" "%83")

  (cons "%bytes_to_string" "caml_string_of_bytes")
  (cons "%bytes_of_string" "caml_bytes_of_string")

  (cons "%identity" "%")
  (cons "%ignore" "%99")
  (cons "%field0" "%67")
  (cons "%field1" "%68")
  (cons "%setfield0" "%73")
  (cons "%array_length" "%79")
  (cons "%array_safe_get" "caml_array_get")
  (cons "%array_safe_set" "caml_array_set")
  (cons "%array_unsafe_get" "caml_array_unsafe_get")
  (cons "%array_unsafe_set" "caml_array_unsafe_set")
  (cons "%floatarray_length" "%79")
  (cons "%floatarray_safe_get" "caml_floatarray_get")
  (cons "%floatarray_safe_set" "caml_floatarray_set")
  (cons "%floatarray_unsafe_get" "caml_floatarray_unsafe_get")
  (cons "%floatarray_unsafe_set" "caml_floatarray_unsafe_set")

  (cons "%obj_size" "%79")
  (cons "%obj_field" "caml_array_unsafe_get")
  (cons "%obj_set_field" "caml_array_unsafe_set")
  (cons "%obj_is_int" "%129")

  (cons "%int64_neg" "caml_int64_neg")
  (cons "%int64_add" "caml_int64_add")
  (cons "%int64_sub" "caml_int64_sub")
  (cons "%int64_mul" "caml_int64_mul")
  (cons "%int64_div" "caml_int64_div")
  (cons "%int64_mod" "caml_int64_mod")
  (cons "%int64_and" "caml_int64_and")
  (cons "%int64_or" "caml_int64_or")
  (cons "%int64_xor" "caml_int64_xor")
  (cons "%int64_lsl" "caml_int64_shift_left")
  (cons "%int64_asr" "caml_int64_shift_right")
  (cons "%int64_lsr" "caml_int64_shift_right_unsigned")
  (cons "%int64_of_int" "caml_int64_of_int")
  (cons "%int64_to_int" "caml_int64_to_int")
  (cons "%int64_of_int32" "caml_int64_of_int32")
  (cons "%int64_to_int32" "caml_int64_to_int32")
  (cons "%int64_of_nativeint" "caml_int64_of_nativeint")
  (cons "%int64_to_nativeint" "caml_int64_to_nativeint")

  (cons "%nativeint_neg" "caml_nativeint_neg")
  (cons "%nativeint_add" "caml_nativeint_add")
  (cons "%nativeint_sub" "caml_nativeint_sub")
  (cons "%nativeint_mul" "caml_nativeint_mul")
  (cons "%nativeint_div" "caml_nativeint_div")
  (cons "%nativeint_mod" "caml_nativeint_mod")
  (cons "%nativeint_and" "caml_nativeint_and")
  (cons "%nativeint_or" "caml_nativeint_or")
  (cons "%nativeint_xor" "caml_nativeint_xor")
  (cons "%nativeint_lsl" "caml_nativeint_shift_left")
  (cons "%nativeint_asr" "caml_nativeint_shift_right")
  (cons "%nativeint_lsr" "caml_nativeint_shift_right_unsigned")
  (cons "%nativeint_of_int" "caml_nativeint_of_int")
  (cons "%nativeint_to_int" "caml_nativeint_to_int")
  (cons "%nativeint_of_int32" "caml_nativeint_of_int32")
  (cons "%nativeint_to_int32" "caml_nativeint_to_int32")

  (cons "%int32_neg" "caml_int32_neg")
  (cons "%int32_add" "caml_int32_add")
  (cons "%int32_sub" "caml_int32_sub")
  (cons "%int32_mul" "caml_int32_mul")
  (cons "%int32_div" "caml_int32_div")
  (cons "%int32_mod" "caml_int32_mod")
  (cons "%int32_and" "caml_int32_and")
  (cons "%int32_or" "caml_int32_or")
  (cons "%int32_xor" "caml_int32_xor")
  (cons "%int32_lsl" "caml_int32_shift_left")
  (cons "%int32_asr" "caml_int32_shift_right")
  (cons "%int32_lsr" "caml_int32_shift_right_unsigned")
  (cons "%int32_of_int" "caml_int32_of_int")
  (cons "%int32_to_int" "caml_int32_to_int")
))
(define prims #nil)
(define nprims 0)
(define (cprim name arity)
  (set! prims (cons name prims))
  (set! nprims (+ 1 nprims))
  (let ((primnum (- nprims 1)))
    (cond ((= arity 1) (list C_CALL1 primnum))
          ((= arity 2) (list C_CALL2 primnum))
          ((= arity 3) (list C_CALL3 primnum))
          ((= arity 4) (list C_CALL4 primnum))
          ((= arity 5) (list C_CALL5 primnum))
          (else (list C_CALLN arity primnum)))))

(define (raw-prim name arity)
  (if (equal? (string-ref name 0) #\%)
      (let* ((l (map string->number (if (string=? name "%") #nil (string-split (substring name 1) #\,)))))
        (if (member #f l) (errorp "Unknown primitive: " name))
        l)
      (cprim name arity)))

(define (prim name arity)
  (let ((p (assoc name known-prims)))
    (raw-prim (if (pair? p) (cdr p) name) arity)
))

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
(define BRANCHIF 85)
(define BRANCHIFNOT 86)
(define SWITCH 87)
(define PUSHTRAP 89)
(define POPTRAP 90)
(define CHECKSIGNALS 92)
(define C_CALL1 93)
(define C_CALL2 94)
(define C_CALL3 95)
(define C_CALL4 96)
(define C_CALL5 97)
(define C_CALLN 98)
(define CONSTINT 103)
(define NEQ 122)
(define LTINT 123)
(define GTINT 125)
(define OFFSETINT 127)
(define ISINT 129)
(define BNEQ 132)
(define STOP 143)
(define RERAISE 146)
(define obj_tag (prim "caml_obj_tag" 1))
(define string_equal (prim "caml_string_equal" 2))

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
  (mkvar location funshape prim)
  var?
  (location var-get-location)
  (funshape var-get-funshape)
  (prim var-get-prim)
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

(define (check-no-labels shape) (for-each (lambda (lab) (assert (equal? (car lab) 'Nolabel))) shape))

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
          ((null? args) (begin (check-no-labels shape) #nil))
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

(define (split-clauses h)
  (let* (
      (h-values
         (filter-map (match-lambda (('ClValue p g e) (list p g e))
                                   (('ClException p g e) #f)) h))
      (h-exceptions
       (filter-map (match-lambda (('ClException p g e) (list p g e))
                                 (('ClValue p g e) #f)) h))
  ) (list h-values h-exceptions)))

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
  (mkswitch consts strings blocks default nums)
  switch?
  (consts switch-get-consts switch-with-consts)
  (strings switch-get-strings switch-with-strings)
  (blocks switch-get-blocks switch-with-blocks)
  (default switch-get-default switch-with-default)
  (nums switch-get-nums switch-with-nums)
)

(define empty-switch (mkswitch #nil #nil #nil #nil #nil))

(define (switch-cons-const sw const)
  (switch-with-consts sw (cons const (switch-get-consts sw))))
(define (switch-cons-string sw string)
  (switch-with-strings sw (cons string (switch-get-strings sw))))
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

(define-immutable-record-type <switch-string>
  (mkswitch-string string rhs)
  switch-string?
  (string switch-string-get-string)
  (rhs switch-string-get-rhs))

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
;
; Each row of the matrix contains
; - one pattern for each column of the matrix
; - a list of bindings of pattern variables to argument variables
; - the "action", containing the when-guard (if any) and the right-hand-side body of the clause
;
; One matrix corresponding to
;   match x,  y,  z with
;      |  p1 as v, q1, r1 when g -> e1
;      |  p2,      q2 as w, r2 -> e2
; has the "arguments" list (args)
;   (list x y z)
; and the "matrix" of clauses (m)
;   (list
;    (list (list p1 q1 r1) (list (cons v x)) (cons g e1))
;    (list (list p2 q2 r2) (list (cons w y)) (cons #nil e2)))
; The "arity" of a matrix is the size of its (args)
; list, and the number of columns of the matrix --
; the two quantities always remain equal.
;
; The output of the matrix decomposition is a "matching tree"
; (constructors use the 'MT prefix), which is itself turned
; into lower code by the lower-matching-tree function.
;
; Note: in practice we do not store when-guards and actions in the row,
; we represent them by just the index in the initial clauses. This lets us
; detect which actions are duplicated by the matching process, and turn those
; into functions (for guards) and exits (for actions) to avoid code duplication.
(define (lower-match env istail arg h)
  (match-let* (
     (patterns (map car h))
     (actions (map (match-lambda ((p g e) (cons g e))) h))

     (pattern-vars (map pat-vars patterns))
     (actions
      (map (lambda (pat-vars action)
             (match-let* (
                  (env (local-vars env pat-vars))
                  ((g . rhs) action)
              )
              (cons
                 (if (null? g) #nil (lower-expr env #f g))
                 (lower-expr env istail rhs)))
      ) pattern-vars actions))

     (clause-numbers (range 0 (length h)))
     (args (list arg))
     (matrix
      ; a list of clauses is turned into a matrix of arity 1
      (map (lambda (p idx) (list (list p) #nil idx)) patterns clause-numbers))
     (guarded? (lambda (idx) (not (null? (car (list-ref actions idx))))))
     (matching-tree
      (match-decompose-matrix env guarded? args matrix))


     (action-frequencies
      (compute-action-frequencies clause-numbers matching-tree))
     ((under-guard-defs under-exit-defs actions)
      (deduplicate-actions
       (map
        (lambda (vars act freq) (list vars act freq))
         pattern-vars actions action-frequencies)))
  )
  (under-guard-defs
   (under-exit-defs
    (lower-matching-tree matching-tree actions)))
))

(define (pat-vars p)
  (let loop ((p p) (acc #nil))
    (match p
      (('PVar v) (cons v acc))
      (('PAs p v) (loop p (cons v acc)))
      (('PWild) acc)
      ((or ('PInt _) ('PString _)) acc)
      (('PConstr c ps) (fold-right loop acc ps))
      (('POr p1 p2)
       (let* ((vars1 (loop p1 #nil))
              (vars2 (loop p2 #nil)))
         (assert (equal? (sort vars1 string<=?) (sort vars2 string<=?)))
         (append vars1 acc)
       ))
      (('PRecord field-defs)
       (fold-right loop acc (map cdr field-defs)))
      (('PFullRecord field-pats)
       (fold-right loop acc field-pats))
      (('POpen _ p) (loop p acc))
)))

(define (match-decompose-matrix env guarded? args m)
  ; (display "matrix") (display args) (newline)
  ; (for-each (lambda (r) (display r) (newline)) m)
  (assert (or (null? m) (equal? (length args) (length (car (car m))))))
  (match (cons args m)
    ((_ . #nil)
     ; no clauses: this matrix rejects all inputs
     (list 'MTFailure))
    ((#nil . ((#nil bindings action) . rest))
     ; no columns: matching (on nothing) succeeds,
     ; but may still need to continue if a guard fails
     (list 'MTAction bindings action
       (if (not (guarded? action))
         #nil
         (match-decompose-matrix env guarded? args rest))))
    (((arg . args) . m)
     ; arity > 0; we represent m with a distinguished head column
     ; (first column, other columns, bindings, action)
     (let*
      ((m (map (match-lambda (((p . ps) bindings e) (list p ps bindings e))) m))
       (m (simplify-matrix env arg m))
       (groups (split-matrix-in-groups m))
       (matching-tree (match-decompose-groups env guarded? arg args groups)))
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
    (('PString s)
     (list (list (list 'HPString s) #nil ps bindings e)))
    (('PConstr c l)
     (let* ((cdef (env-get-constr env c))
            (arity (constr-get-arity cdef))
            (tag (constr-get-tag cdef))
            (nums (constr-get-numconstrs cdef))
            (l (adjust-pconstr-args l arity))
            (arity (length l)))
       (list (list (list 'HPConstr tag arity nums) l ps bindings e))))
    (('PFullRecord field-pats)
     (list (list (list 'HPRecord (length field-pats)) field-pats ps bindings e)))
    (('POr p1 p2)
     (append
      (simplify-row env arg (list p1 ps bindings e))
      (simplify-row env arg (list p2 ps bindings e))))
    (('POpen m p)
     (let* ((menv (env-get-module env m)))
       (simplify-row (env-open env menv) arg (list p ps bindings e))))
)))

(define (pattern-head-arity h)
  (match h
    (#nil 0)
    ((or ('HPInt _) ('HPString _)) 0)
    (('HPConstr tag arity nums) arity)
    (('HPRecord arity) arity)
  ))

(define (omegas-for-pattern-head h)
  (make-list (pattern-head-arity h) (list 'PWild)))

(define (args-for-pattern-head arg h)
  (map (lambda (i) (string-append arg "#" (number->string i))
     ) (range 0 (pattern-head-arity h))))

(define (split-matrix-in-groups simplified-matrix)
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
       (strong-groups (vhash-delete #nil groups))
       (groups
        ; if the strong groups are complete, no need for the default group
        (if (complete-groups? strong-groups) strong-groups groups))
       )
    groups))

; A set of strong groups is "complete" if all possible constructors
; for values of this type are covered.
(define (complete-groups? strong-groups)
  (let* (
    (group-width
     (if (vlist-null? strong-groups) #nil
       (match (car (vlist-head strong-groups))
         ((or ('HPInt _) ('HPString _)) #nil)
         (('HPRecord arity) 1)
         (('HPConstr tag arity nums)
          (match-let* (
             ((numconsts . numblocks) nums)
           ) (if (< numconsts 0) #nil
                 (+ numconsts numblocks))
           ))
       )))
  ) (and (not (null? group-width))
         (equal? group-width (vlist-length strong-groups)))
))

(define (match-decompose-groups env guarded? arg args groups)
  (list
   'MTSwitch
   arg
   (vhash-map (lambda (h submatrix)
       (let* ((newargs (args-for-pattern-head arg h))
              (matching-tree
               (match-decompose-matrix env guarded? (append newargs args) submatrix)))
       (cons newargs matching-tree))
     ) groups)
))

(define (compute-action-frequencies action-numbers matching-tree)
  (assert (equal? action-numbers (range 0 (length action-numbers))))
  (let ((freqs (make-vector (length action-numbers) 0)))
  (let iter ((matching-tree matching-tree))
    (match matching-tree
      (('MTFailure) #f)
      (('MTAction bindings act rest)
       (vector-set! freqs act (+ 1 (vector-ref freqs act)))
       (if (null? rest) #f (iter rest)))
      (('MTSwitch arg groups)
       (vlist-for-each (match-lambda ((h . (vars . group-tree)) (iter group-tree))) groups))
  ))
  (vector->list freqs)
))

(define (deduplicate-actions action-infos)
  (match-let* (((guard-defs exit-defs actions)
    (fold-right (lambda (action-info i acc)
        (match-let* (
           ((vars action freq) action-info)
           ((guard-defs exit-defs actions) acc)
        )
        (if (<= freq 1)
          (list guard-defs exit-defs (cons action actions))
          (match-let* (
             ((g . e) action)
             ((guard-defs . g)
              (if (or (null? g) (substituable? g))
                (cons guard-defs g)
                (let* (
                  (guard-name (string-append "guard#" (number->string i)))
                  (guard-def
                   (list guard-name (if (null? vars) (list #nil) vars) g))
                  (guard-call
                   (list 'LApply (lid->lvar guard-name)
                      (if (null? vars)
                        (list (list 'LConst 0))
                        (map lid->lvar vars))))
                ) (cons (cons guard-def guard-defs) guard-call))))
             ((exit-defs . e)
              (if (substituable? e)
                (cons exit-defs e)
                (let* (
                  (exit-name (string-append "act#" (number->string i)))
                  (exit-def (list exit-name vars e))
                  (exit-call (list 'LExit exit-name (map lid->lvar vars)))
                ) (cons (cons exit-def exit-defs) exit-call))))
           ) (list guard-defs exit-defs (cons (cons g e) actions))))
      )) (list #nil #nil #nil) action-infos (range 0 (length action-infos)))
  ))
  (list
   (lambda (code)
     (fold-right (lambda (guard-info body)
         (match-let* (((name args fun) guard-info))
           (list 'LLetfun name args fun body))
       ) code guard-defs))
   (lambda (code)
     (if (null? exit-defs) code (list 'LLetexits exit-defs code)))
   actions)
))

(define (lower-matching-tree matching-tree actions)
  (match matching-tree
    (('MTFailure) (list 'LMatchFailure))
    (('MTAction bindings action-number rest)
     (match-let* (
       (under-bindings (lambda (body)
         ; bindings were accumulated into a list during decomposition,
         ; so the earliest binding is at the end of the list. We use 'fold' so that
         ; it ends up at the beginning of the resulting expression.
         (fold (match-lambda* (((v . arg) e)
                 ; note: this Let is a variable rebinding (let x = y in ...),
                 ; with special support in the code generator
                 (list 'LLet v (lid->lvar arg) e)
           )) body bindings)))
       ((action-guard . action-expr) (list-ref actions action-number))
      )
      (if (null? action-guard)
        (under-bindings action-expr)
        (let* ((exit (string-append "guard#" (number->string action-number) "#exit")))
         (list 'LLetexits
            (list (list exit #nil (lower-matching-tree rest actions)))
            (under-bindings
             (list 'LIf action-guard
                action-expr
                (list 'LExit exit #nil))))
        ))
     ))
    (('MTSwitch arg groups) (lower-match-groups arg groups actions))
))

(define (lower-match-groups arg groups actions)
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
                   (lower-matching-tree default-matching-tree actions)))))
         (sw (vhash-fold (lambda (h group sw)
                (match-let* (((group-vars . group-matching-tree) group)
                             (group-code (lower-matching-tree group-matching-tree actions)))
                (match h
                  (('HPInt n)
                   (switch-cons-const sw (mkswitch-const n group-code)))
                  (('HPString s)
                   (switch-cons-string sw (mkswitch-string s group-code)))
                  (('HPRecord arity)
                   (switch-cons-block sw (mkswitch-block 0 arity group-vars group-code)))
                  (('HPConstr tag arity nums)
                   ; TODO: instead of passing argument variables to the switch compiler,
                   ; we could lower the field-access logic by binding the new-args
                   ; with LGetField access.
                   (let* ((sw (switch-merge-nums sw nums)))
                   (if (equal? arity 0)
                       (switch-cons-const sw (mkswitch-const tag group-code))
                       (switch-cons-block sw (mkswitch-block tag arity group-vars group-code)))))
                ))) sw groups)))
  (list 'LSwitch (lid->lvar arg) sw)))

(define (lower-try env istail exn-arg h)
  (lower-match env istail exn-arg
    ; add a reraise clause at the end of the pattern-matching
    (append h (list
      (list (list 'PWild) #nil (list 'EReraise (lid->evar exn-arg)))))))

(define (lower-var env ld check-shape)
  (let* ((vdef (env-get-var env ld))
         (vloc (var-get-location vdef))
         (vshape (var-get-funshape vdef)))
    (if check-shape (check-no-labels vshape))
    (match vloc
           (('VarLocal v)
            (assert (equal? ld (list 'Lident v)))
            (list 'LVar v))
           (('VarGlobal id)
            (list 'LGlobal id))
           )
    ))

(define (lower-expr env istail expr)
  (let ((lower-tail (lambda (e) (lower-expr env istail e)))
        (lower-notail (lambda (e) (lower-expr env #f e))))
  (match expr
    (('EVar ld) (lower-var env ld #t))
    (('EConstant c)
     (match c
       (('CInt (n . #nil))
        (list 'LConst n))
       (('CInt (n . #\L))
        (list 'LGlobal (newglob (list 'Int64 n) (string-append (number->string n) "L"))))
       (('CInt (n . #\l))
        (list 'LGlobal (newglob (list 'Int32 n) (string-append (number->string n) "l"))))
       (('CInt (n . #\n))
        (list 'LGlobal (newglob (list 'Intnat n) (string-append (number->string n) "n"))))
       (('CInt (n . c))
        (errorp "Unknown int extension: " (list->string (list c))))
       (('CUnit)
        (list 'LConst 0))
       (('CString str)
        (list 'LGlobal (newglob (list 'String str) (string-append "\"" str "\""))))))
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
      (match-let* (
         ((f-shape f-expr f-prim) (match f
             (('EVar ld)
              (list (var-get-funshape (env-get-var env ld))
                    (lower-var env ld #f)
                    (var-get-prim (env-get-var env ld))))
             (_ (list #nil (lower-notail f) #nil))))
         (args (align-args f-shape args))
         (args (map lower-notail args)))
       ; (display f-expr)(display f-prim)
       (if (and (pair? f-prim) (= (car f-prim) (length args)))
           (list 'LPrim (cdr f-prim) args)
           (if istail
               (list 'LTailApply f-expr args)
               (list 'LApply f-expr args)))))
    (('EMatch e clauses)
     (match-let* (
         (e (lower-notail e))
         ((value-clauses exception-clauses) (split-clauses clauses))
     ) (if (null? exception-clauses)
           (list 'LLet "match#arg" e
             (lower-match (local-var env "match#arg") istail "match#arg" value-clauses))
           (list 'LCatch e
             "match#arg" (lower-match (local-var env "match#arg") istail "match#arg" value-clauses)
             "match#exn" (lower-try   (local-var env "match#exn") istail "match#exn" exception-clauses)))
           ))
    (('ETry e clauses)
     (match-let* (
            (e (lower-notail e))
            ((value-clauses exception-clauses) (split-clauses clauses))
            (val-var "try#val")
            (exn-var "try#exn")
       )
       (assert (null? exception-clauses))
       (list 'LCatch e
         val-var (lid->lvar val-var)
         exn-var (lower-try (local-var env exn-var) istail exn-var value-clauses))))
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
    (('EWhile cond body) (list 'LWhile (lower-notail cond) (lower-notail body)))
    (('EFor v dir b1 b2 body)
     (let ((env (local-var env v)))
       (list 'LFor v dir (lower-notail b1) (lower-notail b2) (lower-expr env #f body))))
)))

(define (local-var-with-shape env v shape)
  (env-replace-var env v (mkvar (list 'VarLocal v) shape #nil)))
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
             (list (list 'ClValue p #nil (list 'ELet #f rest body))))))
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
  (let* ((noneline (list 'ClValue (lid->pconstr "None" #nil) #nil default))
         (someline (list 'ClValue (lid->pconstr "Some" (list (lid->pvar name))) #nil (lid->evar name)))
         (default-expr (list 'EMatch (lid->evar name) (list noneline someline))))
    (list 'ELet #f (list (cons (lid->pvar name) default-expr)) body)))

(define (lower-arg-pat pat name body)
  (match pat
   (('PVar _)
    body)
   (_
    (list 'EMatch (lid->evar name) (list (list 'ClValue pat #nil body))))))

(define (lower-switch env istail sw)
  (match-let*
   ((($ <switch> consts strings blocks default nums) sw)
    (consts
      (map (match-lambda
            (($ <switch-const> tag rhs)
             (mkswitch-const tag
               (lower-expr env istail rhs)))
         ) consts))
    (strings
      (map (match-lambda
            (($ <switch-string> str rhs)
             (mkswitch-string str
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
   (mkswitch consts strings blocks default nums)))

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
        'LGetfield 'LSetfield
        'LPrim 'LApply 'LTailApply
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
           (bytecode-put-u32-le (newglob (list 'Integer n) "<constant int>")))))
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
    (('LPrim prim-code args)
     (compile-args env stacksize args)
     (for-each bytecode-put-u32-le prim-code))
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
    (('LCatch e val-var val-handler exn-var exn-handler)
     (let* ((lab-exn (newlabel))
            (lab-end (newlabel))
            (stacksize-before-trap stacksize)
            (stacksize-after-trap (+ stacksize 4))
            )
       (bytecode-put-u32-le PUSHTRAP)
       (bytecode-emit-labref lab-exn)
       (compile-expr
        (compenv-push-trap env stacksize-before-trap stacksize-after-trap)
        stacksize-after-trap e)
       (bytecode-put-u32-le POPTRAP)
       (compile-bind-var env stacksize val-var val-handler)
       (bytecode-BRANCH-to lab-end)
       (bytecode-emit-label lab-exn)
       (compile-bind-var env stacksize exn-var exn-handler)
       (bytecode-emit-label lab-end)))
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
    (('LWhile cond body)
     (let* ((labcond (newlabel))
            (labloop (newlabel)))
       (bytecode-BRANCH-to labcond)
       (bytecode-emit-label labloop)
       (bytecode-put-u32-le CHECKSIGNALS)
       (compile-expr env stacksize body)
       (bytecode-emit-label labcond)
       (compile-expr env stacksize cond)
       (bytecode-put-u32-le BRANCHIF)
       (bytecode-emit-labref labloop)
       (bytecode-CONSTINT 0)
       ))
    (('LFor v dir b1 b2 body)
     (let* ((labloop (newlabel))
            (labend (newlabel)))
       (compile-expr env stacksize b1)
       (bytecode-put-u32-le PUSH)
       (compile-expr env (+ stacksize 1) b2)
       (bytecode-put-u32-le PUSH)
       (bytecode-put-u32-le PUSH)
       (bytecode-ACC 2)
       (bytecode-put-u32-le (if (equal? dir 'UpTo) GTINT LTINT))
       (bytecode-put-u32-le BRANCHIF)
       (bytecode-emit-labref labend)
       (bytecode-emit-label labloop)
       (bytecode-put-u32-le CHECKSIGNALS)
       (compile-expr (stack-var env v stacksize) (+ stacksize 2) body)
       (bytecode-ACC 1)
       (bytecode-put-u32-le PUSH)
       (bytecode-put-u32-le OFFSETINT)
       (bytecode-put-u32-le (if (equal? dir 'UpTo) 1 -1))
       (bytecode-put-u32-le ASSIGN)
       (bytecode-put-u32-le 2)
       (bytecode-ACC 1)
       (bytecode-put-u32-le NEQ)
       (bytecode-put-u32-le BRANCHIF)
       (bytecode-emit-labref labloop)
       (bytecode-emit-label labend)
       (bytecode-CONSTINT 0)
       (bytecode-POP 2)
       ))
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
      (match e
        (('LVar v)
         ; this optimizes (let x = e in y) or (catch e (x -> y) (exn -> ...))
         (if (equal? var v)
             ; if 'v' is 'var', there is nothing to do, the value is already on top
             #f
             ; if 'v' is not 'var', we do not need to save 'var'
             (compile-expr env stacksize e)))
        (_
         (bytecode-put-u32-le PUSH)
         (compile-expr (stack-var env var stacksize) (+ stacksize 1) e)
         (bytecode-POP 1)))))

(define (compile-bind-var-pushed env stacksize var e)
  (if (null? var)
      (begin
        (bytecode-POP 1)
        (compile-expr env (- stacksize 1) e))
      (begin
        (compile-expr (stack-var env var (- stacksize 1)) stacksize e)
        (bytecode-POP 1))))

(define (compile-switch env stacksize sw)
  (match-let ((($ <switch> consts strings blocks default nums) sw))
    (cond
     ((and (null? consts) (null? strings) (null? blocks))
      (compile-bind-var env stacksize (car default) (cdr default)))
     ((not (null? strings))
      (assert (and (null? consts) (null? blocks)))
      (compile-string-switch env stacksize sw))
     ((or (null? nums) (< (car nums) 0))
      (compile-fragile-switch env stacksize sw))
     (else
      (compile-variant-switch env stacksize sw)))))

; The matched constructors do not carry representation information
; from their type declaration. This typically occurs when matching on
; integer literals, or exceptions in the (try .. with ...) desugaring.
;
; Inefficient compilation using if and Obj.tag.
(define (compile-fragile-switch env stacksize sw)
  (match-let ((($ <switch> consts #nil blocks default nums) sw))
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
        (for-each bytecode-put-u32-le obj_tag))
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
    (bytecode-emit-label endlab)
)))


(define (compile-string-switch env stacksize sw)
  (match-let ((($ <switch> #nil strings #nil default _) sw))
  (let* ((endlab (newlabel)))
    (bytecode-put-u32-le PUSH)
    (for-each (match-lambda (($ <switch-string> str e)
                (let* ((lab (newlabel)))
                  (bytecode-put-u32-le GETGLOBAL)
                  (bytecode-put-u32-le (newglob (list 'String str) "<constant string switch>"))
                  (bytecode-put-u32-le PUSH)
                  (bytecode-ACC 1)
                  (for-each bytecode-put-u32-le string_equal)
                  (bytecode-put-u32-le BRANCHIFNOT)
                  (bytecode-emit-labref lab)
                  (bytecode-POP 1)
                  (compile-expr env stacksize e)
                  (bytecode-BRANCH-to endlab)
                  (bytecode-emit-label lab)
                  )))
              strings)
    (if (pair? default)
        (compile-bind-var-pushed env (+ stacksize 1) (car default) (cdr default))
        (bytecode-put-u32-le STOP))
    (bytecode-emit-label endlab)
)))

(define (compile-variant-switch env stacksize sw)
  (match-let ((($ <switch> consts #nil blocks default nums) sw))
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
    (('LSetfield e1 i e2)
     (vset-union (fv-expr e1 bv) (fv-expr e2 bv)))
    (('LPrim c es)
     (fv-expr-list es bv))
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
    (('LCatch e v1 e1 v2 e2)
       (vset-union
        (fv-expr e bv)
        (vset-union
         (fv-expr e1 (bv-add-var v1 bv))
         (fv-expr e2 (bv-add-var v2 bv)))))
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
            (argss (map cadr bindings))
            (funs (map caddr bindings))
            (rec-bv (bv-add-vars vars bv)))
       (vset-union
        (vset-list-union (map (lambda (fun args) (fv-expr fun (bv-add-vars args rec-bv))) funs argss))
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
    (('LWhile cond body)
     (vset-union
      (fv-expr cond bv)
      (fv-expr body bv)))
    (('LFor v dir b1 b2 body)
     (vset-union (fv-expr b1 bv)
     (vset-union (fv-expr b2 bv)
                 (fv-expr body (bv-add-var v bv)))))
  ))

(define (fv-expr-list exprs bv)
  (vset-list-union
   (map (lambda (e) (fv-expr e bv)) exprs)))

(define (fv-switch sw bv)
  (match-let ((($ <switch> consts strings blocks default nums) sw))
    (vset-list-union (append
      (map (match-lambda
            (($ <switch-const> tag rhs)
             (fv-expr rhs bv))
         ) consts)
      (map (match-lambda
            (($ <switch-string> str rhs)
             (fv-expr rhs bv))
         ) strings)
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
    (bytecode-put-u32-le CLOSUREREC)8
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
    (let* ((locations (map (lambda (def) (if (equal? (def-get-name def) "_") #nil (slot-for-global (def-get-name def)))) bindings))
           (nenv-vars (fold (match-lambda* ((($ <def> name args body) loc e)
                             (let ((shape (map arg-get-label args)))
                               (if (equal? name "_") e
                                   (bindings-replace
                                      name (mkvar (list 'VarGlobal loc) shape #nil)
                                      e))))
                         ) (env-get-vars env) bindings locations))
           (nenv (env-with-vars env nenv-vars))
           (tenv (if rec-flag nenv env)))
      (for-each (match-lambda* ((($ <def> name args body) loc)
                    (if show-progress (begin (display "Compiling ") (display name) (newline)))
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
           (prim-code (prim primname arity))
           (lab1 (newlabel))
           (lab2 (newlabel))
           (pos (slot-for-global name)))
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
      (for-each bytecode-put-u32-le prim-code)
      (bytecode-put-u32-le RETURN)
      (bytecode-put-u32-le arity)
      (bytecode-emit-label lab1)
      (bytecode-put-u32-le CLOSURE)
      (bytecode-put-u32-le 0)
      (bytecode-emit-labref lab2)
      (bytecode-put-u32-le SETGLOBAL)
      (bytecode-put-u32-le pos)
      (env-replace-var env name (mkvar (list 'VarGlobal pos) shape (cons arity prim-code)))
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
  (newglob (list 'Integer exnid) "<builtin exn>"))

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

(define (caml-capitalize name)
  (let ((s (string->list name)))
    (list->string (cons (char-upcase (car s)) (cdr s)))))

(define (input-file->module-name file)
  (cond
   ((string-suffix? ".ml" file) (caml-capitalize (basename file ".ml")))
   ((string-suffix? ".mli" file) (caml-capitalize (basename file ".mli")))
   (else (errorp "Unknown file extension"))))

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

(define show-progress #f)

(define (process-args args)
  (match args
         (#nil '())
         (("-h" . rest) (usage-and-exit))
         (("-P" . rest) (set! show-progress #t) (process-args rest))
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
(define exec-end (list 'MLet #f (list (mkdef "_" #nil (mkapp1 "__atexit" (list 'EConstant (list 'CUnit)))))))

(bytecode-open-output output-file)

(bytecode-begin-section "CODE")
(compile-defs initial-env (append prog (list exec-end)))
(bytecode-put-u32-le STOP)

(bytecode-begin-section "PRIM")
(bytecode-write-prims)

(bytecode-begin-section "DATA")
(bytecode-write-globals)

(bytecode-begin-section "SYMB")
(bytecode-write-symbols)

(bytecode-close-output)
