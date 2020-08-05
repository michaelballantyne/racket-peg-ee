#lang racket/base

(require
  "core.rkt"
  (for-syntax racket/base syntax/parse))

(provide #%peg-datum)

(define-syntax #%peg-datum
  (peg-macro
    (syntax-parser
      [(_ e:string)
       #'(token (lambda (t) (values (and (equal? t e) t) #f)))])))
