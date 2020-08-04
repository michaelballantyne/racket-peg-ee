#lang racket

(require
  "core.rkt"
  (for-syntax syntax/parse))

(provide symbol)

(define-syntax symbol
  (peg-macro
   (syntax-parser
     [(_ s:id)
      #'(token (lambda (t)
                 (values (and (eq? t 's) t) #f)))])))