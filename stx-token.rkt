#lang racket/base

(require
  "core.rkt"
  syntax/srcloc
  (for-syntax racket/base syntax/parse))

(provide #%peg-datum)

(define-syntax #%peg-datum
  (peg-macro
   (syntax-parser
     [(_ x:string)
      (define/syntax-parse x-sym (string->symbol (syntax->datum #'x)))
      #'(token (lambda (s)
                 (if (equal? 'x-sym (syntax-e s))
                     (values s (build-source-location s))
                     (values #f #f))))])))

