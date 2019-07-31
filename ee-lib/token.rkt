#lang racket

(require "peg.rkt"
         (for-syntax
          syntax/parse))

(provide define-tokens #%peg-datum)

(define (token-runtime name)
  (lambda (v)
    (if (eq? (car v) name)
        (values #t (if (pair? (cdr v)) (cadr v) (void)))
        (values #f (void)))))

(begin-for-syntax
  (define (make-token-transformer sym)
    (syntax-parser
      [_:id
       #`(-token-pred (token-runtime '#,sym))])))

(define-syntax define-tokens
  (syntax-parser
    [(_ token:id ...)
     #`(begin
         (define-peg-macro token (make-token-transformer 'token))
         ...)]))

(define (token-datum-runtime str)
  (lambda (v)
    (match v
      [(or `(KEYWORD ,s) `(PUNCT ,s))
       (values (equal? str s) s)]
      [_ (values #f (void))])))

(define-peg-macro #%peg-datum
  (syntax-parser
    [(_ s:string)
     #'(-token-pred (token-datum-runtime 's))]))