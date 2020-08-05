#lang racket/base

(provide (all-defined-out))

(require
  syntax/parse
  ee-lib
  "env-reps.rkt")

(define-syntax-class peg
  #:description "PEG expression"
  (pattern e))

(define-syntax-class nonterm-id
  #:description "PEG non-terminal name"
  (pattern n:id
           #:fail-unless
           (not (eq? unbound (lookup #'n parser-binding?)))
           "not bound as a peg"))