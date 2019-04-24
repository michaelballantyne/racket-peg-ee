#lang racket

(require (for-syntax racket/syntax))
(require racket/performance-hint)

(provide define-peg parse ~eps ~char ~or ~seq ~* ~!)

(define-syntax ~eps #f)
(define-syntax ~char #f)
(define-syntax ~or #f)
(define-syntax ~seq #f)
(define-syntax ~* #f)
(define-syntax ~! #f)

(begin-encourage-inline
  (define-inline (char-rt in ix c)
    (and (< ix (string-length in))
         (char=? c (string-ref in ix))
         (+ ix 1))))

(begin-for-syntax
  (struct parser [f])
  
  (define (compile e in ix)
    (with-syntax ([in in] [ix ix])
      (syntax-case e (~eps ~char ~or ~seq ~* ~!)
        [~eps
         #'ix]
        [(~char c)
         #'(char-rt in ix c)]
        [(~or e1 e2)
         (with-syntax ([c1 (compile #'e1 #'in #'ix)]
                       [c2 (compile #'e2 #'in #'ix)])
           
           #'(or c1 c2))]
        [(~seq e1 e2)
         (with-syntax ([ix^ (generate-temporary 'ix)])
           (with-syntax ([c1 (compile #'e1 #'in #'ix)]
                         [c2 (compile #'e2 #'in #'ix^)])
             #'(let ([ix^ c1])
                 (and ix^ c2))))]
        ; can't encode as [r (or (seq e r) eps)] due to loss of tail recursion from `or`
        [(~* e)
         (with-syntax ([c (compile #'e #'in #'ix^)])
           #'(letrec ([f (lambda (ix^)
                           (let ([ix^^ c])
                             (if ix^^
                                 (f ix^^)
                                 ix^)))])
               (f ix)))]
        ; TODO: test
        [(~! e)
         (with-syntax ([c (compile #'e #'in #'ix)])
           #'(if c ix #f))]
        [nonterm
         (identifier? #'nonterm)
         (with-syntax ([f (syntax-local-introduce (parser-f (syntax-local-value #'nonterm)))])
           #'(f in ix))]
        ))))

(define-syntax (define-peg stx)
  (syntax-case stx ()
    [(_ name e)
     (with-syntax ([f (generate-temporary #'name)])
       (with-syntax ([body (compile #'e #'in #'ix)])
         #'(begin
             (define (f in ix) body)
             (define-syntax name (parser #'f)))))]))

(define-syntax (parse stx)
  (syntax-case stx ()
    [(_ nonterm str-e)
     (with-syntax ([f (parser-f (syntax-local-value #'nonterm))])
       #'(f str-e 0))]))

