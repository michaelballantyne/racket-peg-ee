#lang racket

(require
  "../core.rkt"
  "../symbol-token.rkt")

(struct binop-ast [e1 op e2])

(define-peg arith-expr-leftrec
  (alt (symbol term)
       (=> (seq (: e1 arith-expr-leftrec)
                (seq
                 (: op (alt (symbol +) (symbol -)))
                 (: e2 (symbol term))))
           (binop-ast e1 op e2))))