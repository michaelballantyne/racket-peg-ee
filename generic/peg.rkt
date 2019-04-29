#lang racket

(provide
 define-peg parse
 -eps -or -seq -* -! -& -local
 define-peg-macro
 define-simple-peg-macro
 -bind -action
 -+)

(require
  racket/undefined
  
  syntax-generic2/define
  (for-syntax
   racket/syntax
   syntax-generic2
   (rename-in syntax/parse [define/syntax-parse def/stx])))

; Distinguished type to represent failure, such that anything else
; can be used as the input.
(struct failure [])
(define (fail) (values (failure) (void)))

(module+ internal
  (provide
   fail
   failure
   (for-syntax
    peg-expand
    peg-compile
    dispatch-peg-expand
    dispatch-peg-compile)))

(begin-for-syntax
  (define-syntax-generic parser-f)
  (define-syntax-generic peg-expand)
  (define-syntax-generic peg-compile)
  (define-syntax-generic peg-macro)

  (define (parser f-stx)
    (generics
     [parser-f (lambda (_) f-stx)]))

  (define (dispatch-peg-compile e in)
    (syntax-parse e
      [_ #:when (peg-compile? e)
         (apply-as-transformer peg-compile 'expression #f e in)]))

  (define (dispatch-peg-expand e [ctx #f])
    (syntax-parse e
      [_ #:when (peg-expand? e)
         (apply-as-transformer peg-expand 'expression ctx e)]
      [_ #:when (peg-macro? e)
         (dispatch-peg-expand
          (apply-as-transformer peg-macro 'expression ctx e))]
      [nonterm:id (dispatch-peg-expand #'(#%peg-call nonterm) ctx)]
      [(head . rest) (raise-syntax-error #f "not a peg expression" e)]
      [d (with-syntax ([#%peg-datum (datum->syntax #'d '#%peg-datum)])
           (dispatch-peg-expand #'(#%peg-datum d) ctx))])))

(define-syntax -eps
  (generics/parse -eps
    [(peg-expand) (values this-syntax '())]
    [(peg-compile in) #`(values in (void))]))

(define-syntax/generics (-or e1 e2)
  [(peg-expand)
   (define-values (e1^ v1) (dispatch-peg-expand #'e1))
   (define-values (e2^ v2) (dispatch-peg-expand #'e2))
   (values (qstx/rc (-or #,e1^ #,e2^)) (append v1 v2))]
  [(peg-compile in)
   (def/stx c1 (dispatch-peg-compile #'e1 in))
   (def/stx c2 (dispatch-peg-compile #'e2 in))
   #'(let-values ([(in res) c1])
       (if (failure? in)
           c2
           (values in res)))])

(define-syntax/generics (-seq e1 e2)
  [(peg-expand)
   (define-values (e1^ v1) (dispatch-peg-expand #'e1))
   (define-values (e2^ v2) (dispatch-peg-expand #'e2))
   (values (qstx/rc (-seq #,e1^ #,e2^)) (append v1 v2))]
  [(peg-compile in)
   (def/stx c1 (dispatch-peg-compile #'e1 in))
   (def/stx c2 (dispatch-peg-compile #'e2 #'in))
   #'(let-values ([(in res) c1])
       (if (failure? in)
           (fail)
           (let-values ([(in res) c2])
             (values in (void)))))])

(define-syntax/generics (-* e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-* #,e^)) '())]
  [(peg-compile in)
   (def/stx c (dispatch-peg-compile #'e #'in))
   #`(letrec ([f (lambda (in)
                   (let-values ([(in^ res^) c])
                     (if (failure? in^)
                         (values in (void))
                         (f in^))))])
       (f #,in))])

(define-syntax/generics (-! e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-! #,e^)) '())]
  [(peg-compile in)
   (def/stx c (dispatch-peg-compile #'e in))
   #`(let-values ([(in res) c])
       (if (failure? in)
           (values #,in (void))
           (fail)))])
   
(define-syntax/generics (-local ([n e]) b)
  [(peg-expand)
   (define ctx (make-def-ctx))
   (def/stx n^ (bind! ctx #'n #'(parser #f)))
   (define-values (e^ ve) (dispatch-peg-expand #'e ctx))
   (define-values (b^ vb) (dispatch-peg-expand #'b ctx))
   (define v (map (lambda (vb) (internal-definition-context-introduce
                                ctx vb 'remove)) vb))
   (values (qstx/rc (-local ([n^ #,e^]) #,b^)) v)]
  [(peg-compile in)
   (def/stx c (dispatch-peg-compile #'e #'in))
   (def/stx bc (dispatch-peg-compile #'b in))
   #`(letrec ([n (lambda (in)
                   c)])
       bc)])

(define-syntax/generics (-action pe e)
  [(peg-expand)
   (define-values (pe^ v) (dispatch-peg-expand #'pe))
   (values (qstx/rc (-action #,pe^ e)) '())]
  [(peg-compile in)
   (define-values (pe^ vs) (dispatch-peg-expand #'pe))
   (def/stx c (dispatch-peg-compile #'pe in))
   (def/stx (v ...) (map syntax-local-introduce vs))
   #'(let ([v undefined] ...)
       (let-values ([(in _) c])
         (if (failure? in)
             (fail)
             (let ([res e])
               (values in res)))))])

(define-syntax/generics (-bind n e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-bind n #,e^)) (list (syntax-local-introduce #'n)))]
  [(peg-compile in)
   (def/stx c (dispatch-peg-compile #'e in))
   #'(let-values ([(in res) c])
       (if (failure? in)
           (fail)
           (begin
             (set! n res)
             (values in res))))])

(define-syntax/generics (#%peg-call nonterm)
  [(peg-expand)
   (when (not (parser-f? #'nonterm))
     (raise-syntax-error #f "not a peg non-terminal" #'nonterm))
   (values this-syntax '())]
  [(peg-compile in)
   (def/stx f (if (parser-f? #'nonterm)
                  (syntax-local-introduce (parser-f #'nonterm)) ; bound to syntax at module level
                  #'nonterm))                                   ; local; rebound in second pass
   #`(f #,in)])

(define-syntax (define-peg stx)
  (syntax-parse stx
    [(_ name:id e)
     (def/stx f (generate-temporary #'name))
     (define-values (e^ v) (dispatch-peg-expand #'e))
     (when (not (null? v))
       (raise-syntax-error #f "top-level must not bind variables" #'e))
     (def/stx body (dispatch-peg-compile
                    e^
                    #'in))
     #'(begin
         (define (f in) body)
         (define-syntax name (parser #'f)))]))

(define-syntax (parse stx)
  (syntax-parse stx
    [(_ nonterm:id in-e)
     (when (not (parser-f? #'nonterm))
       (raise-syntax-error #f "not a peg non-terminal" #'nonterm))
     (def/stx f (syntax-local-introduce (parser-f #'nonterm)))
     #'(let-values ([(in res) (f in-e)])
         (if (failure? in)
             (error 'parse "parse failed")
             res))]))

(define-syntax-rule
  (define-peg-macro name rhs)
  (define-syntax name (generics [peg-macro rhs])))

(require (for-syntax syntax/parse/private/sc))

(begin-for-syntax
  (define-syntax-class head
    (pattern (name:id . rest)
             #:attr pat #'((~var name id) . rest))
    (pattern name:id
             #:attr pat #'(~var name id))))

(define-syntax (define-simple-peg-macro stx)
  (syntax-parse stx
    [(_ h:head . body)
     #`(define-peg-macro h.name
         (syntax-parser/template
          #,((make-syntax-introducer) stx)
          [h.pat . body]))]))


; bad implementation! consumes linear stack due to `or`.
#;(provide -*2)
#;(define-simple-peg-macro
    (-*2 e)
    (-local ([tmp2 (-or (-seq e tmp2) -eps)]) tmp2))

(define-simple-peg-macro
  (-& e)
  (-! (-! e)))

(define-simple-peg-macro
  (-+ e)
  (-local ([tmp e]) (-seq tmp (-* tmp))))
