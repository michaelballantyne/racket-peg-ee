#lang racket

(require
  "core.rkt"
  (for-syntax syntax/parse))

(provide symbol)

(define-syntax symbol
  (peg-macro
   (syntax-parser
     [(_ s:id)
      #'(token (lambda (t) (and (eq? t 's) 's)))])))