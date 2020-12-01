(use-modules (system base lalr)
             (srfi srfi-1) (srfi srfi-9 gnu)
             (rnrs base)
             (ice-9 binary-ports) (ice-9 vlist) (ice-9 match))


(define-immutable-record-type <def>
  (mkdef name args body)
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

(define (mkapp fname args) (list 'EApply (list 'Lident fname) (map mknolabelapp args)))
(define (mkapp1 fname arg1) (mkapp fname (list arg1)))
(define (mkapp2 fname arg1 arg2) (mkapp fname (list arg1 arg2)))
(define (mkapp3 fname arg1 arg2 arg3) (mkapp fname (list arg1 arg2 arg3)))

(define (mklambda args body)
  (match body
         (('ELambda args2 body2) (list 'ELambda (append args args2) body2))
         (_ (list 'ELambda args body))
  ))

(define ml-parser
  (lalr-parser
   (expect: 0)
   ;; Token definitions
   (LPAREN LBRACE RBRACE QUOTE TILDE
           QUESTION SEMICOLONSEMICOLON LBRACK RBRACK LBRACKBAR BARRBRACK
           AND BEGIN END EXCEPTION EXTERNAL FUN FUNCTION IF IN MODULE
           MUTABLE OF OPEN REC STRUCT TRY TYPE WITH
           EOF STRING LIDENT UIDENT INT
           (right: MINUSGT)
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
           (left: EQ LTGT LT GT LTEQ GTEQ BARGT)
           (right: CARET AT ATAT)
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
     (MODULE UIDENT EQ STRUCT list_semidefinition END) : (list 'MStruct $2 $5)
     (EXTERNAL LIDENT COLON type_count_arrows EQ STRING) : (list 'MExternal $2 $4 $6))

   (type_ands
    ( ) : #nil
    (AND typedef type_ands) : (cons $2 $3))

   (type_name_with_args
    (LIDENT) : $1
    (QUOTE LIDENT LIDENT) : $3
    (LPAREN type_ignore RPAREN LIDENT) : $4)

   (typedef
    (type_name_with_args) : (cons $1 (list 'IRebind))
    (type_name_with_args EQ separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $3))
    (type_name_with_args EQ BAR separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $4))
    (type_name_with_args EQ LBRACE separated_semi_opt_field_decl RBRACE) : (cons $1 (list 'IRecord $4))
    (type_name_with_args EQ type_ignore) : (cons $1 (list 'IRebind)))

   (let_ands
    ( ) : #nil
    (AND letdef let_ands) : (cons $2 $3))

   (letdef
    (LPAREN RPAREN EQ expr) : (mkdef "_" #nil $4)
    (LIDENT list_labelled_arg EQ expr) : (mkdef $1 $2 $4))

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
    (expr_no_semi) : (cons $1 #nil)
    (semi_separated_expr_list SEMICOLON expr_no_semi) : (cons $3 $1))

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

   (type_count_arrows
    ( ) : 0
    (MINUSGT type_count_arrows) : (+ 1 $2)
    (longident_lident type_count_arrows) : $2
    (QUOTE type_count_arrows) : $2
    (STAR type_count_arrows) : $2
    (LPAREN type_ignore RPAREN type_count_arrows) : $4)

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
    (longident_lident EQ expr_no_semi) : (cons (cons $1 $3) #nil)
    (record_list_expr SEMICOLON longident_lident EQ expr_no_semi) : (cons (cons $3 $5) $1))

   (pattern_constr_args
    (LIDENT) : (cons $1 #nil)
    (LIDENT COMMA pattern_constr_args) : (cons $1 $3))

   (comma_separated_list2_lident
    (LIDENT COMMA LIDENT) : (cons $3 (cons $1 #nil))
    (comma_separated_list2_lident COMMA LIDENT) : (cons $3 $1))

   (comma_separated_list2_expr
    (expr_no_semi COMMA expr_no_semi) : (cons $3 (cons $1 #nil))
    (comma_separated_list2_expr COMMA expr_no_semi) : (cons $3 $1))

   (pattern
    (simple_pattern) : $1
    (longident_constr LIDENT) : (list 'PConstr $1 (cons $2 #nil))
    (longident_constr LPAREN pattern_constr_args RPAREN) : (list 'PConstr $1 $3)
    (comma_separated_list2_lident) : (list 'PConstr (list 'Lident "") (reverse $1))
    (LIDENT COLONCOLON LIDENT) : (list 'PConstr (list 'Lident "Cons") (cons $1 (cons $3 #nil))))

   (simple_pattern
    (LIDENT) : (list 'PVar $1)
    (longident_constr) : (list 'PConstr $1 #nil)
    (LBRACK RBRACK) : (list 'PConstr (list 'Lident "Null") #nil)
    (LPAREN pattern COLON type_ignore RPAREN) : $2
    (LPAREN RPAREN) : (list 'PInt 0)
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
    (LBRACE expr WITH record_list_expr option_semicolon RBRACE) : (list 'ERecordwith $2 (reverse $4))
    (LBRACK RBRACK) : (list 'EConstr (list 'Lident "Null") #nil)
    (LBRACK semi_separated_expr_list_opt RBRACK) :
        (fold-right (lambda (e r) (list 'EConstr (list 'Lident "Cons") (list e r))) (list 'EConstr (list 'Lident "Null") #nil) $2)
    (LBRACKBAR BARRBRACK) : (list 'EVar (list 'Ldot (list 'Lident "Array") "empty_array"))
    (LBRACKBAR semi_separated_expr_list_opt BARRBRACK) : (list 'EConstr (list 'Lident "") $2)
    (BANG simple_expr) : (mkapp1 "ref_get" $2)
    (simple_expr DOT LPAREN expr RPAREN) : (mkapp2 "array_get" $1 $4)
    (simple_expr DOT LBRACK expr RBRACK) : (mkapp2 "string_get" $1 $4))

   (labelled_simple_expr
    (simple_expr) : (mknolabelapp $1)
    (TILDE LIDENT (prec: label_prec)) : (cons (list 'EVar (list 'Lident $2)) (list 'Labelled $2))
    (QUESTION LIDENT (prec: label_prec)) : (cons (list 'EVar (list 'Lident $2)) (list 'Optional $2))
    (TILDE LIDENT COLON simple_expr) : (cons $4 (list 'Labelled $2))
    (QUESTION LIDENT COLON simple_expr) : (cons $4 (list 'Optional $2)))

   (nonempty_list_lident
    (LIDENT) : (cons $1 #nil)
    (LIDENT nonempty_list_lident) : (cons $1 $2))

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
    (simple_expr DOT longident_lident LTMINUS expr_no_semi) : (list 'ESetfield $1 $3 $5)
    (IF expr THEN expr ELSE expr) : (list 'EIf $2 $4 $6)
    (IF expr THEN expr) : (list 'EIf $2 $4 (list 'EConstant (list 'CUnit)))
    (expr_no_semi EQ expr_no_semi) : (mkapp2 "eq" $1 $3)
    (expr_no_semi LTGT expr_no_semi) : (mkapp2 "neq" $1 $3)
    (expr_no_semi LT expr_no_semi) : (mkapp2 "lessthan" $1 $3)
    (expr_no_semi GT expr_no_semi) : (mkapp2 "lessthan" $3 $1)
    (expr_no_semi LTEQ expr_no_semi) : (mkapp2 "lessequal" $1 $3)
    (expr_no_semi GTEQ expr_no_semi) : (mkapp2 "lessequal" $3 $1)
    (expr_no_semi PLUS expr_no_semi) : (mkapp2 "plus" $1 $3)
    (expr_no_semi MINUS expr_no_semi) : (mkapp2 "minus" $1 $3)
    (expr_no_semi STAR expr_no_semi) : (mkapp2 "times" $1 $3)
    (expr_no_semi COLONEQ expr_no_semi) : (mkapp2 "ref_set" $1 $3)
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
            (list (mknolabelfun (list 'PVar "arg#function")))
            (list 'EMatch (list 'EVar (list 'Lident "arg#function")) $2))
    (LET llet llet_ands IN expr (prec: LET)) : (list 'ELet #f (cons $2 $3) $5)
    (LET REC llet llet_ands IN expr (prec: LET)) : (list 'ELet #t (cons $3 $4) $6)
    (LET OPEN longident_uident IN expr (prec: LET)) : (list 'ELetOpen $3 $5)
    (longident_uident DOT LPAREN expr RPAREN) : (list 'ELetOpen $1 $4)
    (expr_no_semi COLONCOLON expr_no_semi) : (list 'EConstr (list 'Lident "Cons") (cons $1 (cons $3 #nil)))
    (expr_no_semi CARET expr_no_semi) : (mkapp2 "string_concat" $1 $3)
    (expr_no_semi AT expr_no_semi) : (mkapp2 "list_concat" $1 $3)
    (simple_expr DOT LPAREN expr RPAREN LTMINUS expr_no_semi) : (mkapp3 "array_set" $1 $4 $7)
    (simple_expr DOT LBRACK expr RBRACK LTMINUS expr_no_semi) : (mkapp3 "string_set" $1 $4 $7)
    )

   (expr
    (expr_no_semi) : $1
    (expr SEMICOLON expr) : (list 'EChain $1 $3))

   (llet
    (pattern EQ expr) : (cons $1 $3)
    (LIDENT nonempty_list_labelled_arg EQ expr) : (cons (list 'PVar $1) (mklambda $2 $4)))

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
    (cons "begin" (cons 'BEGIN #f))
    (cons "else" (cons 'ELSE #f))
    (cons "end" (cons 'END #f))
    (cons "exception" (cons 'EXCEPTION #f))
    (cons "external" (cons 'EXTERNAL #f))
    (cons "false" (cons 'UIDENT "false"))
    (cons "fun" (cons 'FUN #f))
    (cons "function" (cons 'FUNCTION #f))
    (cons "if" (cons 'IF #f))
    (cons "in" (cons 'IN #f))
    (cons "let" (cons 'LET #f))
    (cons "match" (cons 'MATCH #f))
    (cons "module" (cons 'MODULE #f))
    (cons "mutable" (cons 'MUTABLE #f))
    (cons "of" (cons 'OF #f))
    (cons "open" (cons 'OPEN #f))
    (cons "rec" (cons 'REC #f))
    (cons "struct" (cons 'STRUCT #f))
    (cons "then" (cons 'THEN #f))
    (cons "true" (cons 'UIDENT "true"))
    (cons "try" (cons 'TRY #f))
    (cons "type" (cons 'TYPE #f))
    (cons "with" (cons 'WITH #f))
    ))

(define (get-lident s)
  (let ((p (assoc s kw)))
    (if p (cdr p) (cons 'LIDENT s))))

(define (mktoken location tk) (make-lexical-token (car tk) location (cdr tk)))

(define (comment errorp)
  (let* ((location (make-source-location "*stdin*" (port-line (current-input-port)) (port-column (current-input-port)) -1 -1))
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
  (let* ((location (make-source-location "*stdin*" (port-line (current-input-port)) (port-column (current-input-port)) -1 -1))
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
  (let* ((location (make-source-location "*stdin*" (port-line (current-input-port)) (port-column (current-input-port)) -1 -1))
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

(define (skip-until-newline)
  (let ((c (read-char)))
    (cond ((eof-object? c) '())
          ((char=? c #\newline) '())
          (else (skip-until-newline))
    )))

(define (token errorp)
  (let* ((location (make-source-location "*stdin*" (port-line (current-input-port)) (port-column (current-input-port)) -1 -1))
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
        ((char=? c #\|) (cond
                         ((char=? (peek-char) #\])
                          (read-char) (make-lexical-token 'BARRBRACK location #f))
                         ((char=? (peek-char) #\|)
                          (read-char) (make-lexical-token 'BARBAR location #f))
                         ((char=? (peek-char) #\>)
                          (read-char) (make-lexical-token 'BARGT location #f))
                         (else (make-lexical-token 'BAR location #f))))
        ((char=? c #\;) (if (char=? (peek-char) #\;)
                            (begin (read-char) (make-lexical-token 'SEMICOLONSEMICOLON location #f))
                            (make-lexical-token 'SEMICOLON location #f)))
        ((char=? c #\=) (make-lexical-token 'EQ location #f))
        ((char=? c #\.) (make-lexical-token 'DOT location #f))
        ((char=? c #\:) (if (char=? (peek-char) #\:)
                            (begin (read-char) (make-lexical-token 'COLONCOLON location #f))
                            (if (char=? (peek-char) #\=)
                                (begin (read-char) (make-lexical-token 'COLONEQ location #f))
                                (make-lexical-token 'COLON location #f))))
        ((char=? c #\+) (make-lexical-token 'PLUS location #f))
        ((char=? c #\-) (if (char=? (peek-char) #\>)
                            (begin (read-char) (make-lexical-token 'MINUSGT location #f))
                            (if (char-numeric? (peek-char))
                                (make-lexical-token 'INT location (- (string->number (list->string (number-chars errorp)))))
                                (make-lexical-token 'MINUS location #f))))
        ((char=? c #\*) (make-lexical-token 'STAR location #f))
        ((char=? c #\~) (make-lexical-token 'TILDE location #f))
        ((char=? c #\@) (cond
                         ((char=? (peek-char) #\@)
                          (read-char) (make-lexical-token 'ATAT location #f))
                         (else (make-lexical-token 'AT location #f))))
        ((char=? c #\^) (make-lexical-token 'CARET location #f))
        ((char=? c #\?) (make-lexical-token 'QUESTION location #f))
        ((char=? c #\!) (make-lexical-token 'BANG location #f))
        ((char=? c #\&) (if (char=? (peek-char) #\&)
                            (begin (read-char) (make-lexical-token 'AMPERAMPER location #f))
                            (errorp "Illegal character: " c)))
        ((char=? c #\<) (if (char=? (peek-char) #\>)
                            (begin (read-char) (make-lexical-token 'LTGT location #f))
                            (if (char=? (peek-char) #\=)
                                (begin (read-char) (make-lexical-token 'LTEQ location #f))
                                (if (char=? (peek-char) #\-)
                                    (begin (read-char) (make-lexical-token 'LTMINUS location #f))
                                    (make-lexical-token 'LT location #f)))))
        ((char=? c #\>) (if (char=? (peek-char) #\=)
                            (begin (read-char) (make-lexical-token 'GTEQ location #f))
                            (make-lexical-token 'GT location #f)))
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
    (newline))))

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
    (put-string bytecode-output-port "Caml1999X025")
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
      (cons #f (string->number (substring name 1)))
      (begin
        (set! prims (cons name prims))
        (set! nprims (+ 1 nprims))
        (cons #t (- nprims 1)))))
(define (bytecode-write-prims)
  (for-each (lambda (name) (begin (bytecode-put-string name) (bytecode-put-u8 0))) (reverse prims)))


(define ACC 8)
(define PUSH 9)
(define POP 19)
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
(define obj_tag (prim "caml_obj_tag"))

(define-immutable-record-type <env>
  (mkenv vars constrs fields modules)
  env?
  (vars env-get-vars env-with-vars)
  (constrs env-get-constrs env-with-constrs)
  (fields env-get-fields env-with-fields)
  (modules env-get-modules env-with-modules))


(define (env-replace-var env k v)
  (env-with-vars env (vhash-replace k v (env-get-vars env))))
(define (env-replace-constr env k v)
  (env-with-constrs env (vhash-replace k v (env-get-constrs env))))
(define (env-replace-field env k v)
  (env-with-fields env (vhash-replace k v (env-get-fields env))))
(define (env-replace-module env k v)
  (env-with-modules env (vhash-replace k v (env-get-modules env))))

(define empty-env (mkenv vlist-null vlist-null vlist-null vlist-null))

(define (vhash-assoc-err name env)
  (let ((r (vhash-assoc name env)))
    (if (pair? r) r (errorp "Not found in env: " name))
    ))

(define (env-get-module env ld)
  (match ld
    (('Lident v)
     (cdr (cdr (vhash-assoc-err v (env-get-modules env)))))
    (('Ldot ld uid)
     (cdr (cdr (vhash-assoc-err uid (env-get-modules (env-get-module env ld))))))))

(define (env-get-env-li env ld)
  (match ld
    (('Lident v)
     (cons env v))
    (('Ldot ld uid)
     (cons (env-get-module env ld) uid))))

(define (env-get-var env ld)
  (let ((envs (env-get-env-li env ld)))
    (cdr (cdr (vhash-assoc-err (cdr envs) (env-get-vars (car envs)))))))
(define (env-get-constr env ld)
  (let ((envs (env-get-env-li env ld)))
    (cdr (cdr (vhash-assoc-err (cdr envs) (env-get-constrs (car envs)))))))
(define (env-get-field env ld)
  (let ((envs (env-get-env-li env ld)))
    (cdr (cdr (vhash-assoc-err (cdr envs) (env-get-fields (car envs)))))))

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
                  (e (cond ((null? r) (list 'EConstr (list 'Lident "None") #nil))
                           ((equal? (car (cdr (car r))) 'Labelled) (list 'EConstr (list 'Lident "Some") (cons (car (car r)) #nil)))
                           (else (car (car r))))))
             (cons e (align (cdr shape) nargs))))
          ))
  ; (newline)(display funshape)(newline)(display args)(newline)

  (align funshape args))

(define rec-closure-step 2)

(define (access-var location stacksize)
  (match location
    (('VarStack pos)
     (bytecode-put-u32-le ACC)
     (bytecode-put-u32-le (- (- stacksize pos) 1)))
    (('VarEnv pos)
     (bytecode-put-u32-le ENVACC)
     (bytecode-put-u32-le (+ 1 pos)))
    (('VarRec pos)
     (bytecode-put-u32-le OFFSETCLOSURE)
     (bytecode-put-u32-le pos))
    (('VarGlobal id)
     (bytecode-put-u32-le GETGLOBAL)
     (bytecode-put-u32-le id))))

(define (adjust-constr-args args arity)
  (cond ((and (= arity 1) (> (length args) 1))
         (list 'EConstr (list 'Lident "") args))
        ((and (> arity 1) (= (length args) 1))
         (match args
           ((('EConstr ('Lident "") args)) args)
           (_ args)))
        (else args)))

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

(define (split-pattern-matching env m)
  (match m
    (#nil empty-switch)
    (((p . e) . rest)
     (match p
       (('PVar v)
        (switch-with-default empty-switch (cons v e)))
       (('PInt n)
        (let* ((sw-rest (split-pattern-matching env rest))
               (sw (switch-no-nums sw-rest)))
          (switch-cons-const sw (mkswitch-const n e))))
       (('PConstr c l)
        (let* ((sw-rest (split-pattern-matching env rest))
               (cdef (env-get-constr env c))
               (arity (constr-get-arity cdef))
               (tag (constr-get-tag cdef))
               (cnums (constr-get-numconstrs cdef))
               (const (mkswitch-const tag e))
               (block (mkswitch-block tag arity l e))
               (sw (switch-merge-nums sw-rest cnums)))
          (match l
            (#nil
             (assert (or (= arity 0) (= arity -1)))
             (switch-cons-const sw const))
            (("_")
              (switch-cons-block sw block))
            (_
             (assert (or (= arity -1) (= arity (length l))))
             (switch-cons-block sw block)))))))))

(define (localvar-with-shape env var pos shape)
  (env-replace-var env var (cons #t (mkvar (list 'VarStack pos) shape))))

(define (localvar env var pos) (localvar-with-shape env var pos #nil))

(define (compile-bind-fields env stacksize istail vars e)
  (match-let (((i j env) (fold
             (match-lambda* ((var (i j env))
               (if (equal? var "_")
                   (list (+ i 1) j env)
                   (let* ((nenv (localvar env var (+ stacksize j))))
                     (bytecode-put-u32-le ACC)
                     (bytecode-put-u32-le j)
                     (bytecode-put-u32-le GETFIELD)
                     (bytecode-put-u32-le i)
                     (bytecode-put-u32-le PUSH)
                     (list (+ i 1) (+ j 1) nenv)))
               )) (list 0 0 env) vars)))
    (compile-expr env (+ stacksize j) istail e)
    (bytecode-put-u32-le POP)
    (bytecode-put-u32-le (+ j 1))
  ))

(define (compile-bind-var env stacksize istail var e)
  (if (equal? var "_")
      (compile-expr env stacksize istail e)
      (begin
        (bytecode-put-u32-le PUSH)
        (compile-expr (localvar env var stacksize) (+ stacksize 1) istail e)
        (bytecode-put-u32-le POP)
        (bytecode-put-u32-le 1))))

(define (compile-bind-var-with-shape env stacksize istail var e shape)
  (bytecode-put-u32-le PUSH)
  (compile-expr (localvar-with-shape env var stacksize shape) (+ stacksize 1) istail e)
  (bytecode-put-u32-le POP)
  (bytecode-put-u32-le 1))


(define (compile-bind-var-pushed env stacksize istail var e)
  (if (equal? var "_")
      (begin
        (bytecode-put-u32-le POP)
        (bytecode-put-u32-le 1)
        (compile-expr env (- stacksize 1) istail e))
      (begin
        (compile-expr (localvar env var (- stacksize 1)) stacksize istail e)
        (bytecode-put-u32-le POP)
        (bytecode-put-u32-le 1))))

(define (compile-matching env stacksize istail m isexn)
  (match-let ((($ <switch> consts blocks default nums)
               (split-pattern-matching env m)))
    (if (and (null? consts) (null? blocks))
        (compile-bind-var env stacksize istail (car default) (cdr default))
        (if (or (null? nums) (< (car nums) 0))
            ; Inefficient compilation using if and Obj.tag
            (let* ((labblock (newlabel))
                   (labdef (newlabel))
                   (endlab (newlabel))
                   (exntype (and (pair? nums) (= (car nums) -2))))
              (bytecode-put-u32-le PUSH)
              (bytecode-put-u32-le ISINT)
              (bytecode-put-u32-le BRANCHIFNOT)
              (bytecode-emit-labref labblock)
              (bytecode-put-u32-le ACC)
              (bytecode-put-u32-le 0)
              (for-each (match-lambda (($ <switch-const> i e)
                          (let* ((lab (newlabel)))
                            (bytecode-put-u32-le BNEQ)
                            (bytecode-put-u32-le i)
                            (bytecode-emit-labref lab)
                            (bytecode-put-u32-le POP)
                            (bytecode-put-u32-le 1)
                            (compile-expr env stacksize istail e)
                            (bytecode-put-u32-le BRANCH)
                            (bytecode-emit-labref endlab)
                            (bytecode-emit-label lab)
                            )))
                        consts)
              (bytecode-put-u32-le BRANCH)
              (bytecode-emit-labref labdef)
              (bytecode-emit-label labblock)
              (bytecode-put-u32-le ACC)
              (bytecode-put-u32-le 0)
              (if exntype
                  (begin
                    (bytecode-put-u32-le GETFIELD)
                    (bytecode-put-u32-le 0))
                  (begin
                    (bytecode-put-u32-le C_CALL1)
                    (bytecode-put-u32-le (cdr obj_tag))))
              (for-each (match-lambda (($ <switch-block> i _ l e)
                          (let* ((lab (newlabel)))
                            (bytecode-put-u32-le BNEQ)
                            (bytecode-put-u32-le i)
                            (bytecode-emit-labref lab)
                            (compile-bind-fields env (+ stacksize 1) istail (if exntype (cons "_" l) l) e)
                            (bytecode-put-u32-le BRANCH)
                            (bytecode-emit-labref endlab)
                            (bytecode-emit-label lab)
                            )))
                        blocks)
              (bytecode-emit-label labdef)
              (if (pair? default)
                  (compile-bind-var-pushed env (+ stacksize 1) istail (car default) (cdr default))
                  (if isexn
                      (begin
                        (bytecode-put-u32-le ACC)
                        (bytecode-put-u32-le 0)
                        (bytecode-put-u32-le RERAISE))
                      (bytecode-put-u32-le STOP)))
              (bytecode-emit-label endlab))
            (let* ((numconsts (car nums))
                   (numblocks (cdr nums))
                   (endlab (newlabel))
                   (defaultlab (newlabel))
                   (constslabs (map (lambda (c) (cons (newlabel) c)) consts))
                   (blockslabs (map (lambda (b) (cons (newlabel) b)) blocks))
                   (basepos (+ (bytecode-get-pos) 8))
                   )
              (assert (not isexn))
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
                          (compile-expr env stacksize istail e)
                          (bytecode-put-u32-le BRANCH)
                          (bytecode-emit-labref endlab)
                          )) constslabs)
              (for-each (match-lambda ((lab . ($ <switch-block> _ _ l e))
                          (bytecode-emit-label lab)
                          (bytecode-put-u32-le PUSH)
                          (compile-bind-fields env (+ stacksize 1) istail l e)
                          (bytecode-put-u32-le BRANCH)
                          (bytecode-emit-labref endlab)
                          )) blockslabs)
              (bytecode-emit-label defaultlab)
              (if (pair? default)
                  (compile-bind-var env stacksize istail (car default) (cdr default))
                  (bytecode-put-u32-le STOP)) ; match failure
              (bytecode-emit-label endlab)
              ))
    )))

(define (lower-expr env istail expr)
  (match expr
    (('EConstant c)
     (match c
       (('CInt n)
        (list 'LConst n))
       (('CUnit)
        (list 'LConst 0))
       (('CString id)
        (list 'LGlobal (newglob id)))))
    (('EConstr name args)
     (match-let ((($ <constr> arity tag nums) (env-get-constr env name)))
        (if (null? args)
            (begin
              (assert (= arity 0))
              (list 'LConst tag))
            (let* ((nargs (adjust-constr-args args arity)))
              (assert (or (= arity (length nargs)) (= arity -1)))
              (if (= (car nums) -2)
                  (list 'LBlock 0 (cons (list 'LConst tag) nargs))
                  (list 'LBlock tag nargs))))))
    (('ERecord l)
        (let* ((size (length l))
               (lf (map (lambda (fe)
                          (match-let ((($ <field> index numfields) (env-get-field env (car fe))))
                            (assert (= numfields size))
                            (cons index (cdr fe)))) l))
               (sf (sort lf (lambda (fe1 fe2) (< (car fe1) (car fe2)))))
               (es (map cdr sf)))
          (for-each (lambda (fe i) (assert (= (car fe) i))) sf (range 0 size))
          (list 'LBlock 0 es)))
    (other other)
))

(define (compile-expr env stacksize istail expr)
  (compile-low-expr env stacksize istail
    (lower-expr env istail expr)))

(define (compile-low-expr env stacksize istail expr)
  ; (newline)(show-env env)(newline)(display stacksize)(newline)(display istail)(newline)(display expr)(newline)
  (match expr
    (('EVar v)
     (access-var (var-get-location (env-get-var env v)) stacksize))
    (('LGlobal id)
     (bytecode-put-u32-le GETGLOBAL)
     (bytecode-put-u32-le id))
    (('LConst n)
     (if (and (<= -1073741824 n) (< n 1073741823))
         (begin
           (bytecode-put-u32-le CONSTINT)
           (bytecode-put-u32-le n))
         (begin
           (bytecode-put-u32-le GETGLOBAL)
           (bytecode-put-u32-le (newglob n)))))
    (('LBlock tag args)
     (compile-args env stacksize args)
     (bytecode-put-u32-le MAKEBLOCK)
     (bytecode-put-u32-le (length args))
     (bytecode-put-u32-le tag))
    (('EGetfield e f)
     (compile-expr env stacksize #f e)
     (bytecode-put-u32-le GETFIELD)
     (bytecode-put-u32-le (field-get-index (env-get-field env f))))
    (('ESetfield e1 f e2)
     (compile-expr env stacksize #f e2)
     (bytecode-put-u32-le PUSH)
     (compile-expr env (+ 1 stacksize) #f e1)
     (bytecode-put-u32-le SETFIELD)
     (bytecode-put-u32-le (field-get-index (env-get-field env f))))
    (('ERecordwith e l)
     (let* ((size (field-get-numfields (env-get-field env (car (car l)))))
            (lf (map (lambda (fe)
                       (match-let ((($ <field> index numfields) (env-get-field env (car fe))))
                         (assert (= numfields size))
                         (cons index (cdr fe)))) l)))
          (assert (> size 0))
          (compile-expr env stacksize #f e)
          (for-each (lambda (i)
                      (let* ((j (- (- size 1) i))
                             (p (find (lambda (fe) (= (car fe) j)) lf))
                             )
                        (bytecode-put-u32-le PUSH)
                        (if (pair? p)
                            (compile-expr env (+ stacksize (+ i 1)) #f (cdr p))
                            (begin
                              (bytecode-put-u32-le ACC)
                              (bytecode-put-u32-le i)
                              (bytecode-put-u32-le GETFIELD)
                              (bytecode-put-u32-le j)))
                        )) (range 0 size))
          (bytecode-put-u32-le MAKEBLOCK)
          (bytecode-put-u32-le size)
          (bytecode-put-u32-le 0)
          (bytecode-put-u32-le POP)
          (bytecode-put-u32-le 1)))
    (('EApply f args1)
      (match-let* ((($ <var> f-location f-shape) (env-get-var env f))
                   (args (align-args f-shape args1))
                   (nargs (length args)))
        (assert (> nargs 0))
        (if istail
            (begin
              (compile-args env stacksize args)
              (bytecode-put-u32-le PUSH)
              (access-var f-location (+ stacksize nargs))
              (bytecode-put-u32-le APPTERM)
              (bytecode-put-u32-le nargs)
              (bytecode-put-u32-le (+ stacksize nargs)))
            (let ((lab (newlabel)))
              (bytecode-put-u32-le PUSH_RETADDR)
              (bytecode-emit-labref lab)
              (compile-args env (+ stacksize 3) args)
              (bytecode-put-u32-le PUSH)
              (access-var f-location (+ stacksize (+ 3 nargs)))
              (bytecode-put-u32-le APPLY)
              (bytecode-put-u32-le nargs)
              (bytecode-emit-label lab)))
        ))
    (('EIf e1 e2 e3)
      (let* ((lab1 (newlabel))
             (lab2 (newlabel)))
        (compile-expr env stacksize #f e1)
        (bytecode-put-u32-le BRANCHIFNOT)
        (bytecode-emit-labref lab1)
        (compile-expr env stacksize istail e2)
        (bytecode-put-u32-le BRANCH)
        (bytecode-emit-labref lab2)
        (bytecode-emit-label lab1)
        (compile-expr env stacksize istail e3)
        (bytecode-emit-label lab2)))
    (('EChain e1 e2)
     (compile-expr env stacksize #f e1)
     (compile-expr env stacksize istail e2))
    (('EMatch e m)
     (compile-expr env stacksize #f e)
     (compile-matching env stacksize istail m #f))
    (('ETry e m)
       (let* ((lab1 (newlabel))
              (lab2 (newlabel)))
         (bytecode-put-u32-le PUSHTRAP)
         (bytecode-emit-labref lab1)
         (compile-expr env (+ stacksize 4) #f e)
         (bytecode-put-u32-le POPTRAP)
         (bytecode-put-u32-le BRANCH)
         (bytecode-emit-labref lab2)
         (bytecode-emit-label lab1)
         (compile-matching env stacksize istail m #t)
         (bytecode-emit-label lab2)))
    (('ELet rec-flag bindings body)
       (if rec-flag
           (let* ((nenv (compile-recfundefs env stacksize bindings)))
             (compile-expr nenv (+ stacksize (length bindings)) istail body)
             (bytecode-put-u32-le POP)
             (bytecode-put-u32-le (length bindings)))
       ; HACK: sequential let!
          (match bindings
             (#nil
              (compile-expr env stacksize istail body))
             (((('PVar f) . ('ELambda args fbody)) . rest)
              (compile-fundef env stacksize args fbody)
              (compile-bind-var-with-shape
               env stacksize istail f (list 'ELet rec-flag rest body) (map arg-get-label args)))
             (((p . e) . rest)
              (compile-expr env stacksize istail
                 (list 'EMatch e (list (cons p (list 'ELet rec-flag rest body)))))))))
    (('ELetOpen m e)
     (let* ((menv (env-get-module env m)))
       (compile-expr (env-open env menv) stacksize istail e)))
    (('ELambda args body)
     (compile-fundef env stacksize args body))))

(define (compile-expr-list env stacksize l)
  (if (not (null? l))
      (begin
        (compile-expr env stacksize #f (car l))
        (if (not (null? (cdr l)))
            (begin
              (bytecode-put-u32-le PUSH)
              (compile-expr-list env (+ stacksize 1) (cdr l))))
        )))

(define (compile-args env stacksize l) (compile-expr-list env stacksize (reverse l)))


(define (vhash-map proc l) (alist->vhash (map (lambda (kv) (cons (car kv) (proc (car kv) (cdr kv)))) (vlist->list l))))
(define (vhash-filter-map proc l) (alist->vhash (filter-map (lambda (kv) (let ((r (proc (car kv) (cdr kv)))) (if r (cons (car kv) r) r))) (vlist->list l))))
(define (vhash-replace key value l) (vhash-cons key value (vhash-delete key l)))

(define vset-empty (alist->vhash #nil))
(define (vset-singleton x) (alist->vhash (cons (cons x #t) #nil)))
(define (vset-mem x s) (pair? (vhash-assoc x s)))
(define (vset-union s1 s2) (vhash-fold (lambda (k v s) (vhash-replace k v s)) s1 s2))
(define (vset-difference s1 s2) (vhash-fold (lambda (k v s) (vhash-delete k s)) s1 s2))
(define (vset-list-union l) (fold vset-union vset-empty l))

(define (expr-fv expr env)
  (match expr
    (('EVar v)
     (let* ((loc (var-get-location (env-get-var env v))))
       (if (equal? (car loc) 'VarGlobal)
           vset-empty
           (begin
             (assert (equal? (car v) 'Lident))
             (vset-singleton (car (cdr v)))))))
    (('EConstant _)
     vset-empty)
    (('EConstr c args)
     (vset-list-union (map (lambda (e) (expr-fv e env)) args)))
    (('EGetfield e f)
     (expr-fv e env))
    (('ESetfield e1 e2)
     (vset-union (expr-fv e1 env) (expr-fv e2 env)))
    (('ERecord args)
     (vset-list-union (map (lambda (fe) (expr-fv (cdr fe) env)) args)))
    (('ERecordwith e args)
     (vset-union (expr-fv e env) (vset-list-union (map (lambda (fe) (expr-fv (cdr fe) env)) args))))
    (('EApply f args)
     (vset-union (expr-fv (list 'EVar f) env) (vset-list-union (map (lambda (e) (expr-fv (car e) env)) args))))
    (('EIf e1 e2 e3)
     (vset-union (expr-fv e1 env)
     (vset-union (expr-fv e2 env)
                 (expr-fv e3 env))))
    (('EChain e1 e2)
     (vset-union (expr-fv e1 env) (expr-fv e2 env)))
    (('EMatch e m)
     (let* ((fv1 (expr-fv e env))
            (lfv (map (lambda (b) (branch-fv b env)) m)))
       (vset-union fv1 (vset-list-union lfv))))
    (('ETry e m)
        (let* ((fv1 (expr-fv e env))
               (lfv (map (lambda (b) (branch-fv b env)) m)))
          (vset-union fv1 (vset-list-union lfv))))
    (('ELet rec-flag bindings body)
     (if rec-flag
         (let* ((nenv (fold fv-env-pat env (map car bindings))))
           (vset-union (vset-list-union (map (lambda (b) (expr-fv (cdr b) nenv)) bindings)) (expr-fv body nenv)))
         (match bindings
           (#nil
            (expr-fv body env))
           (((p . e) . rest)
            (expr-fv (list 'EMatch e (list (cons p (list 'ELet rec-flag rest body)))) env)))))
    (('ELetOpen m e)
     (let* ((menv (env-get-module env m)))
       (expr-fv e (env-open env menv))))
    (('ELambda args body)
         (let ((pats (map arg-get-pat args)))
           (expr-fv body (fold fv-env-pat env pats))))))

(define (expr-fv-binding expr env args)
  (expr-fv expr (fold fv-env-var env args)))

(define (branch-fv b env)
  (match-let (((p . e) b))
    (expr-fv e (fv-env-pat p env))))

(define (fv-env-var arg env)
  (env-replace-var env arg (cons #t (mkvar (list 'VarGlobal "dummy") "dummy"))))

(define (fv-env-pat p env)
  (match p
   (('PVar v)
    (fv-env-var v env))
   (('PInt _)
    env)
   (('PConstr c l)
    (fold fv-env-var env l))))


(define (range a b) (if (>= a b) #nil (cons a (range (+ a 1) b))))

(define (get-fun-body args arg-names basebody)
  (fold-right (lambda (arg name body) (match arg
     (($ <arg> pat ('Optional label) ('Some default))
      (compile-arg-default label default body))
     (($ <arg> pat _ ('None))
      (compile-arg-pat pat name body))))
   basebody args arg-names))

(define (compile-fundef-body env arg-names body nfv lab recvars recshapes recoffset)
  (let* ((arity (length arg-names))
         (envoff (* rec-closure-step (- (- (length recvars) 1) recoffset)))
         (mvars (vhash-filter-map
                 (lambda (name v)
                   (if (equal? (car (var-get-location (cdr v))) 'VarGlobal)
                       v
                       (let ((r (vhash-assoc name nfv)))
                         (if (pair? r)
                             (cons (car v) (mkvar (list 'VarEnv (+ envoff (cdr r))) (var-get-funshape (cdr v))))
                             #f))))
                 (env-get-vars env)))
         (rvars (fold (lambda (rec-name shape i vs)
                        (vhash-replace
                         rec-name
                         (cons #t (mkvar (list 'VarRec (* rec-closure-step (- i recoffset))) shape))
                         vs))
                      mvars recvars recshapes (range 0 (length recvars))))
         (nvars (fold (lambda (arg-name i vs)
                        (vhash-replace
                         arg-name
                         (cons #t (mkvar (list 'VarStack (- (- arity 1) i)) #nil))
                         vs))
                      rvars arg-names (range 0 arity)))
         (nenv (env-with-vars env nvars))
         )
    (assert (> arity 0))
    (bytecode-put-u32-le RESTART)
    (bytecode-emit-label lab)
    (bytecode-put-u32-le GRAB)
    (bytecode-put-u32-le (- arity 1))
    (compile-expr nenv arity #t body)
    (bytecode-put-u32-le RETURN)
    (bytecode-put-u32-le arity)
  ))

(define (fv-list fv) (map car (vlist->list fv)))
(define (make-nfv fv) (fold (lambda (name i nfv) (vhash-replace name i nfv)) vlist-null fv (range 0 (length fv))))

(define (compile-fundef env stacksize args basebody)
  (let* ((arity (length args))
         (arg-names (map compile-arg-name args (range 0 arity)))
         (body (get-fun-body args arg-names basebody))
         (fv (fv-list (expr-fv-binding body env arg-names)))
         (nfv (make-nfv fv))
         (lab1 (newlabel))
         (lab2 (newlabel))
         )
    (compile-args env stacksize (map (lambda (v) (list 'EVar (list 'Lident v))) fv))
    (bytecode-put-u32-le BRANCH)
    (bytecode-emit-labref lab1)
    (compile-fundef-body env arg-names body nfv lab2 #nil #nil -1)
    (bytecode-emit-label lab1)
    (bytecode-put-u32-le CLOSURE)
    (bytecode-put-u32-le (length fv))
    (bytecode-emit-labref lab2)
    ))

(define (compile-recfundefs env stacksize funs)
  (let* ((numfuns (length funs))
         (names (map (match-lambda ((('PVar name) . _) name) (_ (assert #f))) funs))
         (args (map (match-lambda ((_ . ('ELambda args _)) args) (_ (assert #f))) funs))
         (shapes (map (lambda (arg) (map arg-get-label arg)) args))
         (basebodies (map (match-lambda ((_ . ('ELambda _ body)) body) (_ (assert #f))) funs))
         (arg-names (map (lambda (arg) (map compile-arg-name arg (range 0 (length arg)))) args))
         (bodies (map get-fun-body args arg-names basebodies))
         (fvenv (fold fv-env-var env names))
         (fv (fv-list (vset-list-union
                       (map (lambda (body arg-name) (expr-fv-binding body fvenv arg-name)) bodies arg-names))))
         (nfv (make-nfv fv))
         (labs (map (lambda (_) (newlabel)) funs))
         (endlab (newlabel))
         (nenv (fold (lambda (name shape pos env) (localvar-with-shape env name (+ stacksize pos) shape))
                     env names shapes (range 0 numfuns)))
         )
    (compile-args env stacksize (map (lambda (v) (list 'EVar (list 'Lident v))) fv))
    (bytecode-put-u32-le BRANCH)
    (bytecode-emit-labref endlab)
    (for-each (lambda (arg-name body lab pos)
                (compile-fundef-body env arg-name body nfv lab names shapes pos))
              arg-names bodies labs (range 0 numfuns))
    (bytecode-emit-label endlab)
    (bytecode-put-u32-le CLOSUREREC)
    (bytecode-put-u32-le numfuns)
    (bytecode-put-u32-le (length fv))
    (let* ((basepos (bytecode-get-pos)))
      (for-each (lambda (lab) (bytecode-emit-labref-with-pos lab basepos)) labs))
    nenv
  ))

(define (compile-arg-name a i)
  (match (arg-get-pat a)
     (('PVar v)
      v)
     (_
      (string-append "arg#" (number->string i)))))

(define (compile-arg-default name default body)
  (let* ((noneline (cons (list 'PConstr (list 'Lident "None") #nil) default))
         (someline (cons (list 'PConstr (list 'Lident "Some") (list name))
                         (list 'EVar (list 'Lident name))))
         (default-expr (list 'EMatch (list 'EVar (list 'Lident name)) (list noneline someline))))
    (list 'ELet #f (list (cons (list 'PVar name) default-expr)) body)))

(define (compile-arg-pat pat name body)
  (if (equal? (car pat) 'PVar)
      body
      (list 'EMatch
            (list 'EVar (list 'Lident name))
            (list (cons pat body)))))

(define empty-numtags (cons 0 0))
(define (numtags-next arity numtags)
  (match-let (((const-count . block-count) numtags))
   (if (> arity 0)
       (cons block-count (cons const-count (+ 1 block-count)))
       (cons const-count (cons (+ 1 const-count) block-count)))))

(define (compile-type env name tdef)
  (match tdef
   (('ISum l)
    (let* ((final-numtags
            (fold (match-lambda* (((name . arity) cur-numtags)
                    (match-let (((next-tag . next-numtags) (numtags-next arity cur-numtags)))
                      next-numtags)))
                  empty-numtags l))
           (nenv-constrs
            (car (fold (match-lambda* (((name . arity) (e . cur-numtags))
                         (match-let (((next-tag . next-numtags) (numtags-next arity cur-numtags)))
                           (cons
                            (vhash-replace name (cons #t (mkconstr arity next-tag final-numtags)) e)
                            next-numtags))))
                       (cons (env-get-constrs env) empty-numtags) l))))
      ; (newline)(display name)(newline)(display numtags)(newline)(newline)
      (env-with-constrs env nenv-constrs)))
    (('IRecord l)
         (let* ((numfields (length l))
                (nenv-fields (car (fold (match-lambda* ((name (e . i))
                                          (cons (vhash-replace name (cons #t (mkfield i numfields)) e)
                                                (+ 1 i))))
                                        (cons (env-get-fields env) 0) l))))
           (env-with-fields env nenv-fields)))
    (('Rebind)
     env)))

(define exnid 0)
(define (declare-exn name arity env)
  (set! exnid (+ 1 exnid))
  (env-replace-constr env name (cons #t (mkconstr arity exnid (cons -2 -2)))))

(define (env-open env menv)
  (let* ((add-bindings (lambda (e me) (vhash-fold-right (lambda (k v ne)
                                                          (if (car v) (vhash-replace k (cons #f (cdr v)) ne) ne))
                                                        e me)))
         (nenv-vars (add-bindings (env-get-vars env) (env-get-vars menv)))
         (nenv-constrs (add-bindings (env-get-constrs env) (env-get-constrs menv)))
         (nenv-fields (add-bindings (env-get-fields env) (env-get-fields menv)))
         (nenv-modules (add-bindings (env-get-modules env) (env-get-modules menv))))
    (mkenv nenv-vars nenv-constrs nenv-fields nenv-modules)))

(define (compile-def env d)
  (match d
   (('MOpen m)
    (let* ((menv (env-get-module env m)))
      (env-open env menv)))
   (('MException name arity)
    (declare-exn name arity env))
   (('MLet rec-flag bindings)
    (let* ((locations (map (lambda (def) (if (equal? (def-get-name def) "_") #nil (slot-for-global))) bindings))
           (nenv-vars (fold (match-lambda* ((($ <def> name args body) loc e)
                             (if (equal? name "_") e
                                 (vhash-replace name
                                             (cons #t (mkvar (list 'VarGlobal loc) (map arg-get-label args))) e))))
                           (env-get-vars env) bindings locations))
                (nenv (env-with-vars env nenv-vars))
                (tenv (if rec-flag nenv env)))
      (for-each (match-lambda* ((($ <def> name args body) loc)
                    ; (display "Compiling ") (display name) (newline)
                    (if (null? args)
                        (compile-expr tenv 0 #f body)
                        (compile-fundef tenv 0 args body))
                    (if (not (null? loc))
                        (begin (bytecode-put-u32-le SETGLOBAL)
                               (bytecode-put-u32-le loc)))
                  )) bindings locations)
      nenv
      ))
   (('MTypedef tdefs)
    (fold (lambda (tdef env) (compile-type env (car tdef) (cdr tdef))) env tdefs))
   (('MStruct name l)
    (let* ((mark (lambda (e) (vhash-map (lambda (k v) (cons #f (cdr v))) e)))
           (modenv (mkenv (mark (env-get-vars env))
                          (mark (env-get-constrs env))
                          (mark (env-get-fields env))
                          (mark (env-get-modules env))))
           (nenv (compile-defs modenv l)))
      (env-replace-module env name (cons #t nenv))
      ))
   (('MExternal name arity primname)
    (let* ((shape (make-list arity (list 'Nolabel)))
           (primnum (prim primname))
           (lab1 (newlabel))
           (lab2 (newlabel))
           (pos (slot-for-global)))
      (assert (> arity 0))
      (bytecode-put-u32-le BRANCH)
      (bytecode-emit-labref lab1)
      (bytecode-put-u32-le RESTART)
      (bytecode-emit-label lab2)
      (bytecode-put-u32-le GRAB)
      (bytecode-put-u32-le (- arity 1))
      (do ((i 0 (1+ i))) ((>= i (- arity 1)))
        (begin
          (bytecode-put-u32-le ACC)
          (bytecode-put-u32-le (- arity 1))
          (bytecode-put-u32-le PUSH)))
      (bytecode-put-u32-le ACC)
      (bytecode-put-u32-le (- arity 1))
      (if (car primnum)
          (cond ((= arity 1) (bytecode-put-u32-le C_CALL1))
                ((= arity 2) (bytecode-put-u32-le C_CALL2))
                ((= arity 3) (bytecode-put-u32-le C_CALL3))
                ((= arity 4) (bytecode-put-u32-le C_CALL4))
                ((= arity 5) (bytecode-put-u32-le C_CALL5))
                (else (begin
                        (bytecode-put-u32-le C_CALLN)
                        (bytecode-put-u32-le arity)))))
      (bytecode-put-u32-le (cdr primnum))
      (bytecode-put-u32-le RETURN)
      (bytecode-put-u32-le arity)
      (bytecode-emit-label lab1)
      (bytecode-put-u32-le CLOSURE)
      (bytecode-put-u32-le 0)
      (bytecode-emit-labref lab2)
      (bytecode-put-u32-le SETGLOBAL)
      (bytecode-put-u32-le pos)
      (env-replace-var env name (cons #t (mkvar (list 'VarGlobal pos) shape)))
      ))
   ))

(define (compile-defs env defs)
  (match defs
   (#nil
    env)
   ((def . rest)
    (compile-defs (compile-def env def) rest))))

(define initial-env
  (env-replace-constr empty-env "" (cons #t (mkconstr -1 0 (cons 0 1)))))

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

(define input-file "")
(define output-file "out.byte")
(define (usage-and-exit)
  (display "Usage: guile compile.scm input.ml -o output\n")
  (exit))

(define (process-args args)
  (match args
         (#nil '())
         (("-h" . rest) (usage-and-exit))
         (("-o" outfile . rest) (set! output-file outfile) (process-args rest))
         ((infile . rest) (set! input-file infile) (process-args rest))
  ))
(if (null? (cdr (program-arguments))) (usage-and-exit))
(process-args (cdr (program-arguments)))

(set-current-input-port (open-input-file input-file))
(define prog (ml-parser (lambda () (token errorp)) errorp))

(bytecode-open-output output-file)

(bytecode-begin-section "CODE")
(compile-defs initial-env prog)
(bytecode-put-u32-le STOP)

(bytecode-begin-section "PRIM")
(bytecode-write-prims)

(bytecode-begin-section "DATA")
(bytecode-write-globals)

(bytecode-close-output)
