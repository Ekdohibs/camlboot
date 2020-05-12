(use-modules (system base lalr) (srfi srfi-1) (rnrs base) (ice-9 binary-ports))

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
     (EXTERNAL LIDENT COLON type_ignore EQ STRING) : (list 'MExternal $2 $6))

   (type_ands
    ( ) : #nil
    (AND typedef type_ands) : (cons $2 $3))

   (type_name_with_args
    (LIDENT) : $1
    (QUOTE LIDENT LIDENT) : $3
    (LPAREN type_ignore RPAREN LIDENT) : $4)

   (typedef
    (type_name_with_args EQ separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $3))
    (type_name_with_args EQ BAR separated_nonempty_list_bar_constr_decl) : (cons $1 (list 'ISum $4))
    (type_name_with_args EQ LBRACE separated_semi_opt_field_decl RBRACE) : (cons $1 (list 'IRecord $4))
    (type_name_with_args EQ type_ignore) : (cons $1 (list 'IRebind)))

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

(define kw (list
    (cons "and" (cons 'AND #f))
    (cons "begin" (cons 'BEGIN #f))
    (cons "else" (cons 'ELSE #f))
    (cons "end" (cons 'END #f))
    (cons "exception" (cons 'EXCEPTION #f))
    (cons "external" (cons 'EXTERNAL #f))
    (cons "false" (cons 'UIDENT "false"))
    (cons "fun" (cons 'FUN #f))
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
          ((char=? c #\() (if (char=? (peek-char) #*) (begin (read-char) (comment errorp) (comment errorp)) (comment errorp)))
          (else (comment errorp))
  )))

(define (string-chars errorp)
  (let* ((location (make-source-location "*stdin*" (port-line (current-input-port)) (port-column (current-input-port)) -1 -1))
         (c (read-char)))
    (cond ((eof-object? c) (errorp "Unterminated string"))
          ((char=? c #\") #nil)
          ((char=? c #\\ ) (todo))
          (else (cons c (string-chars errorp)))
  )))

(define (char-alphanumeric? c) (or (char-alphabetic? c) (char-numeric? c)))

(define (ident errorp)
  (let ((c (peek-char)))
        (cond ((eof-object? c) #nil)
              ((or (char-alphanumeric? c) (char=? c #\_)) (begin (read-char) (cons c (ident errorp))))
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
        ((or (char=? c #\space) (char=? c #\tab)) (token errorp))
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
        ((char=? c #\|) (if (char=? (peek-char) #\])
                            (begin (read-char) (make-lexical-token 'BARRBRACK location #f))
                            (if (char=? (peek-char) #\|)
                                (begin (read-char) (make-lexical-token 'BARBAR location #f))
                                (make-lexical-token 'BAR location #f))))
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
        ((char=? c #\@) (make-lexical-token 'AT location #f))
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
                                 (todo)
                                 (if (char=? (peek-char) #\')
                                     (begin (read-char) (make-lexical-token 'INT location (char->integer c)))
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

(define (bytecode-write-globals globs)
  (bytecode-marshal (cons 0 globs)))

(display (ml-parser (lambda () (token errorp)) errorp))
(bytecode-open-output "testbyte")
(bytecode-begin-section "CODE")
(bytecode-put-u32-le 103) (bytecode-put-u32-le 1) ; CONSTINT(1)
(bytecode-put-u32-le 93) (bytecode-put-u32-le 0) ; C_CALL1(0)
(bytecode-put-u32-le 9) ; PUSH
(bytecode-put-u32-le 103) (bytecode-put-u32-le 13) ; CONSTINT(13)
(bytecode-put-u32-le 9) ; PUSH
(bytecode-put-u32-le 103) (bytecode-put-u32-le 0) ; CONSTINT(0)
(bytecode-put-u32-le 9) ; PUSH
(bytecode-put-u32-le 53) (bytecode-put-u32-le 0) ; GETGLOBAL(0)
(bytecode-put-u32-le 9) ; PUSH
(bytecode-put-u32-le 8) (bytecode-put-u32-le 3) ; ACC(3)
(bytecode-put-u32-le 96) (bytecode-put-u32-le 1) ; C_CALL4(1)
(bytecode-put-u32-le 8) (bytecode-put-u32-le 0) ; ACC(0)
(bytecode-put-u32-le 93) (bytecode-put-u32-le 2) ; C_CALL1(2)
(bytecode-put-u32-le 143) ; STOP
(bytecode-begin-section "PRIM")
(bytecode-put-string "caml_ml_open_descriptor_out") (bytecode-put-u8 0)
(bytecode-put-string "caml_ml_output") (bytecode-put-u8 0)
(bytecode-put-string "caml_ml_flush") (bytecode-put-u8 0)
(bytecode-begin-section "DATA")
(bytecode-write-globals (list "Hello, world!"))
(bytecode-close-output)
