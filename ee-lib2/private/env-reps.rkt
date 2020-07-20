#lang racket/base

(provide (all-defined-out))

(require racket/generic)

(define-generics parser-binding)
(struct parser-binding-rep ()
  #:methods gen:parser-binding [])

(define-generics peg-macro
  (peg-macro-transform peg-macro stx))
(struct peg-macro-rep (procedure)
  #:extra-constructor-name peg-macro
  #:methods gen:peg-macro
  [(define (peg-macro-transform s stx)
     ((peg-macro-rep-procedure s) stx))])