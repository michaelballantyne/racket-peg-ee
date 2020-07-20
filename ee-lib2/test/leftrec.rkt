#lang racket

(require
  "../core.rkt"
  "../symbol-token.rkt")

; Check that nullability information from earlier modules is available
; to later modules.
(module pre racket/base
  (require
  "../core.rkt"
  "../symbol-token.rkt")

  (provide term)

  (define-peg term (symbol term)))

(require 'pre)

(struct binop-ast [e1 op e2])

(define-peg arith-expr-leftrec
  (alt term
       (=> (seq (: e1 arith-expr-leftrec)
                (seq
                 (: op (alt (symbol +) (symbol -)))
                 (: e2 term)))
           (binop-ast e1 op e2))))