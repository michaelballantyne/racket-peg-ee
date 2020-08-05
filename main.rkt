#lang racket/base

(require
  "core.rkt"
  (for-syntax racket/base syntax/parse))

(provide
  (except-out (all-from-out "core.rkt") seq alt)
  (struct-out parse-result)
  define-peg-syntax-parser
  (rename-out
    [seq* seq]
    [alt* alt])
  ?
  any-char
  char-range)

(define-syntax-rule
  (define-peg-syntax-parser name clause ...)
  (define-syntax name
    (peg-macro
      (syntax-parser
        clause ...))))

(define-peg-syntax-parser seq*
  [(_ p:peg) #'p]
  [(_ p1:peg p+:peg ...+)
   #'(seq p1 (seq* p+ ...))])

(define-peg-syntax-parser alt*
  [(_ p:peg) #'p]
  [(_ p1:peg p+:peg ...+)
   #'(alt p1 (alt* p+ ...))])

(define-peg-syntax-parser ?
  [(_ p:peg) #'(alt p eps)])

(define-peg-syntax-parser any-char
  [_:id #'(char (lambda (x) #t))])

(define-peg-syntax-parser char-range
  [(_ lower:character upper:character) #'(char (lambda (c) (and (>= lower c) (<= c upper))))])

