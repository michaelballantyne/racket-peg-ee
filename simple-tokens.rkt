#lang racket

(require
  "core.rkt"
  (for-syntax syntax/parse))

(provide symbol predicate-token)

(define-syntax symbol
  (peg-macro
   (syntax-parser
     [(_ s:id)
      #'(token (lambda (t)
                 (values (and (eq? t 's) t) #f)))])))

(define-syntax predicate-token
  (peg-macro
    (syntax-parser
      [(_ e:expr)
       #'(token (lambda (t) (values (and (e t) t) #f)))])))

