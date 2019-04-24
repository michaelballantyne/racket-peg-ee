#lang racket

(provide define-peg parse -eps -char -or -seq -* -! -local define-peg-macro -+)

(require
  racket/performance-hint

  syntax-generic2/define
  (for-syntax
   racket/syntax
   syntax-generic2
   (rename-in syntax/parse [define/syntax-parse def/stx])))

(begin-encourage-inline
  (define (advance-input-1 c ix ln col)
    (if c
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))
  
  (define (char-rt in ix ln col c)
    (if (< ix (string-length in))
        (let ([c2 (string-ref in ix)])
          (if (char=? c2 c)
              (advance-input-1 c2 ix ln col)
              (values #f ln col)))
        (values #f ln col))))

(begin-for-syntax
  (define-syntax-generic parser-f)
  (define-syntax-generic peg-expand)
  (define-syntax-generic peg-compile)
  (define-syntax-generic peg-macro)

  (define (parser f-stx)
    (generics
     [parser-f (lambda (_) f-stx)]))

  (define (dispatch-peg-compile e in ix ln col)
    (syntax-parse e
      [_ #:when (peg-compile? e)
         (apply-as-transformer peg-compile 'expression #f e in ix ln col)]))

  (define (dispatch-peg-expand e [ctx #f])
    (syntax-parse e
      [_ #:when (peg-expand? e)
         (apply-as-transformer peg-expand 'expression ctx e)]
      [_ #:when (peg-macro? e)
         (dispatch-peg-expand
          (apply-as-transformer peg-macro 'expression ctx e))]
      [nonterm:id
       #:when (parser-f? #'nonterm ctx)
       (dispatch-peg-expand
        #'(#%peg-call nonterm) ctx)])))

(define-syntax -eps
  (generics/parse -eps
    [(peg-expand) this-syntax]
    [(peg-compile in ix ln col)
     #`(values #,ix #,ln #,col)]))

(define-syntax/generics (-char c:char)
  [(peg-expand) this-syntax]
  [(peg-compile in ix ln col)
   #`(char-rt #,in #,ix #,ln #,col c)])

(define-syntax/generics (-or e1 e2)
  [(peg-expand)
   (def/stx e1^ (dispatch-peg-expand #'e1))
   (def/stx e2^ (dispatch-peg-expand #'e2))
   (qstx/rc (-or e1^ e2^))]
  [(peg-compile in ix ln col)
   (def/stx c1 (dispatch-peg-compile #'e1 in ix ln col))
   (def/stx c2 (dispatch-peg-compile #'e2 in ix ln col))
   #'(let-values ([(ix ln col) c1])
       (if ix
           (values ix ln col)
           c2))])

(define-syntax/generics (-seq e1 e2)
  [(peg-expand)
   (def/stx e1^ (dispatch-peg-expand #'e1))
   (def/stx e2^ (dispatch-peg-expand #'e2))
   (qstx/rc (-seq e1^ e2^))]
  [(peg-compile in ix ln col)
   (def/stx c1 (dispatch-peg-compile #'e1 in ix ln col))
   (def/stx c2 (dispatch-peg-compile #'e2 in #'ix #'ln #'col))
   #'(let-values ([(ix ln col) c1])
       (if ix
           c2
           (values ix ln col)))])

(define-syntax/generics (-* e)
  [(peg-expand)
   (def/stx e^ (dispatch-peg-expand #'e))
   (qstx/rc (-* e^))]
  [(peg-compile in ix ln col)
   (def/stx c (dispatch-peg-compile #'e in #'ix #'ln #'col))
   #`(letrec ([f (lambda (ix ln col)
                   (let-values ([(ix^ ln^ col^) c])
                     (if ix^
                         (f ix^ ln^ col^)
                         (values ix ln col))))])
       (f #,ix #,ln #,col))])

(define-syntax -! #f)

(define-syntax/generics (-local ([n e]) b)
  [(peg-expand)
   (define ctx (make-def-ctx))
   (def/stx n^ (bind! ctx #'n #'(parser #f)))
   (def/stx e^ (dispatch-peg-expand #'e ctx))
   (def/stx b^ (dispatch-peg-expand #'b ctx))
   (qstx/rc (-local ([n^ e^]) b^))]
  [(peg-compile in ix ln col)
   (def/stx c (dispatch-peg-compile #'e #'in #'ix #'ln #'col))
   (def/stx bc (dispatch-peg-compile #'b in ix ln col))
   #`(letrec ([n (lambda (in ix ln col)
                   c)])
       bc)])

(define-syntax/generics (#%peg-call nonterm)
  [(peg-expand)
   (when (not (parser-f? #'nonterm))
     (raise-syntax-error #f "not a peg non-terminal" #'nonterm))
   this-syntax]
  [(peg-compile in ix ln col)
   (def/stx f (if (parser-f? #'nonterm)
                  (syntax-local-introduce (parser-f #'nonterm)) ; bound to syntax at module level
                  #'nonterm))                                   ; local; rebound in second pass
   #`(f #,in #,ix #,ln #,col)])

(define-syntax (define-peg stx)
  (syntax-parse stx
    [(_ name:id e)
     (def/stx f (generate-temporary #'name))
     (def/stx body (dispatch-peg-compile
                    (dispatch-peg-expand
                     #'e)
                    #'in #'ix #'ln #'col))
     #'(begin
         (define (f in ix ln col) body)
         (define-syntax name (parser #'f)))]))

(define-syntax (parse stx)
  (syntax-parse stx
    [(_ nonterm:id str-e)
     (when (not (parser-f? #'nonterm))
       (raise-syntax-error #f "not a peg non-terminal" #'nonterm))
     (def/stx f (syntax-local-introduce (parser-f #'nonterm)))
     #'(f str-e 0 0 0)]))

(define-syntax-rule (define-peg-macro name rhs)
  (define-syntax name (generics [peg-macro rhs])))

(define-peg-macro -+
  (syntax-parser
    [(_ e) #'(-local ([tmp e])
               (-seq tmp (-* tmp)))]))
