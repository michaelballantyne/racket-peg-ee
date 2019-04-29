#lang racket

(provide
 define-peg parse
 -eps -char -string -char-pred -any-char -char-range -or -seq -* -! -& -local
 define-peg-macro
 define-simple-peg-macro
 -bind -action -capture
 -+)

(require
  racket/performance-hint
  racket/undefined
  
  syntax-generic2/define
  (for-syntax
   racket/syntax
   syntax-generic2
   (rename-in syntax/parse [define/syntax-parse def/stx])))

(begin-encourage-inline
  (define (step-input c ix ln col)
    (if (char=? c #\newline)
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))

  (define (char-pred-rt in ix ln col p)
    (if (< ix (string-length in))
        (let ([c (string-ref in ix)])
          (if (p c)
              (let-values ([(ix ln col) (step-input c ix ln col)])
                (values ix ln col (void)))
              (values #f ln col (void))))
        (values #f ln col (void))))

  (define (string-rt in ix ln col s)
    (if (<= (+ ix (string-length s)) (string-length in))
        (let loop ([ix ix] [ln ln] [col col] [s-ix 0])
          (if (< s-ix (string-length s))
              (let ([c (string-ref in ix)])
                (if (char=? c (string-ref s s-ix))
                    (let-values ([(ix ln col) (step-input c ix ln col)])
                      (loop ix ln col (+ s-ix 1)))
                    (values #f ln col (void))))
              (values ix ln col s)))
        (values #f ln col (void)))))

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
      [s:string (dispatch-peg-expand #'(-string s) ctx)]
      [c:char (dispatch-peg-expand #'(-char c) ctx)]
      [nonterm:id (dispatch-peg-expand #'(#%peg-call nonterm) ctx)])))

(define-syntax -eps
  (generics/parse -eps
    [(peg-expand) (values this-syntax '())]
    [(peg-compile in ix ln col)
     #`(values #,ix #,ln #,col)]))

(define-syntax/generics (-char-pred p)
  [(peg-expand) (values this-syntax '())]
  [(peg-compile in ix ln col)
   #`(char-pred-rt #,in #,ix #,ln #,col p)])

(define-syntax/generics (-string s:string)
  [(peg-expand) (values this-syntax '())]
  [(peg-compile in ix ln col)
   #`(string-rt #,in #,ix #,ln #,col s)])

(define-syntax/generics (-or e1 e2)
  [(peg-expand)
   (define-values (e1^ v1) (dispatch-peg-expand #'e1))
   (define-values (e2^ v2) (dispatch-peg-expand #'e2))
   (values (qstx/rc (-or #,e1^ #,e2^)) (append v1 v2))]
  [(peg-compile in ix ln col)
   (def/stx c1 (dispatch-peg-compile #'e1 in ix ln col))
   (def/stx c2 (dispatch-peg-compile #'e2 in ix ln col))
   #'(let-values ([(ix ln col res) c1])
       (if ix
           (values ix ln col res)
           c2))])

(define-syntax/generics (-seq e1 e2)
  [(peg-expand)
   (define-values (e1^ v1) (dispatch-peg-expand #'e1))
   (define-values (e2^ v2) (dispatch-peg-expand #'e2))
   (values (qstx/rc (-seq #,e1^ #,e2^)) (append v1 v2))]
  [(peg-compile in ix ln col)
   (def/stx c1 (dispatch-peg-compile #'e1 in ix ln col))
   (def/stx c2 (dispatch-peg-compile #'e2 in #'ix #'ln #'col))
   #'(let-values ([(ix ln col res) c1])
       (if ix
           c2
           (values ix ln col (void))))])

(define-syntax/generics (-* e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-* #,e^)) '())]
  [(peg-compile in ix ln col)
   (def/stx c (dispatch-peg-compile #'e in #'ix #'ln #'col))
   #`(letrec ([f (lambda (ix ln col)
                   (let-values ([(ix^ ln^ col^ res^) c])
                     (if ix^
                         (f ix^ ln^ col^)
                         (values ix ln col (void)))))])
       (f #,ix #,ln #,col))])

(define-syntax/generics (-! e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-! #,e^)) '())]
  [(peg-compile in ix ln col)
   (def/stx c (dispatch-peg-compile #'e in ix ln col))
   #`(let-values ([(ix ln col res) c])
       (if ix
           (values #f ln col (void))
           (values #,ix #,ln #,col (void))))])
   
(define-syntax/generics (-local ([n e]) b)
  [(peg-expand)
   (define ctx (make-def-ctx))
   (def/stx n^ (bind! ctx #'n #'(parser #f)))
   (define-values (e^ ve) (dispatch-peg-expand #'e ctx))
   (define-values (b^ vb) (dispatch-peg-expand #'b ctx))
   (define v (map (lambda (vb) (internal-definition-context-introduce
                                ctx vb 'remove)) vb))
   (values (qstx/rc (-local ([n^ #,e^]) #,b^)) v)]
  [(peg-compile in ix ln col)
   (def/stx c (dispatch-peg-compile #'e #'in #'ix #'ln #'col))
   (def/stx bc (dispatch-peg-compile #'b in ix ln col))
   #`(letrec ([n (lambda (in ix ln col)
                   c)])
       bc)])

(define-syntax/generics (-action pe e)
  [(peg-expand)
   (define-values (pe^ v) (dispatch-peg-expand #'pe))
   (values (qstx/rc (-action #,pe^ e)) '())]
  [(peg-compile in ix ln col)
   (define-values (pe^ vs) (dispatch-peg-expand #'pe))
   (def/stx c (dispatch-peg-compile #'pe in ix ln col))
   (def/stx (v ...) (map syntax-local-introduce vs))
   #'(let ([v undefined] ...)
       (let-values ([(ix ln col _) c])
         (let ([res e])
           (values ix ln col res))))])

(define-syntax/generics (-bind n e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-bind n #,e^)) (list (syntax-local-introduce #'n)))]
  [(peg-compile in ix ln col)
   (def/stx c (dispatch-peg-compile #'e in ix ln col))
   #'(let-values ([(ix ln col res) c])
       (set! n res)
       (values ix ln col res))])

(define-syntax/generics (-capture e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-capture #,e^)) '())]
  [(peg-compile in ix ln col)
   (def/stx c (dispatch-peg-compile #'e in ix ln col))
   #`(let-values ([(ix ln col res) c])
       (values ix ln col (substring #,in #,ix ix)))])

(define-syntax/generics (#%peg-call nonterm)
  [(peg-expand)
   (when (not (parser-f? #'nonterm))
     (raise-syntax-error #f "not a peg non-terminal" #'nonterm))
   (values this-syntax '())]
  [(peg-compile in ix ln col)
   (def/stx f (if (parser-f? #'nonterm)
                  (syntax-local-introduce (parser-f #'nonterm)) ; bound to syntax at module level
                  #'nonterm))                                   ; local; rebound in second pass
   #`(f #,in #,ix #,ln #,col)])

(define-syntax (define-peg stx)
  (syntax-parse stx
    [(_ name:id e)
     (def/stx f (generate-temporary #'name))
     (define-values (e^ v) (dispatch-peg-expand #'e))
     (when (not (null? v))
       (raise-syntax-error #f "top-level must not bind variables" #'e))
     (def/stx body (dispatch-peg-compile
                    e^
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
     #'(let-values ([(ix ln col res)
                     (f str-e 0 0 0)])
         res)]))

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
  (-char c:char)
  (-char-pred (lambda (v) (char=? c v))))

(define-simple-peg-macro
  -any-char
  (-char-pred (lambda (v) #t)))

(define-simple-peg-macro
  (-char-range c1:char c2:char)
  (-char-pred (lambda (v) (and (char>=? v c1) (char<=? v c2)))))

(define-simple-peg-macro
  (-& e)
  (-! (-! e)))

(define-simple-peg-macro
  (-+ e)
  (-local ([tmp e]) (-seq tmp (-* tmp))))
