#lang racket

(require (except-in "../peg.rkt" #%peg-datum)
         "../token.rkt"
         rackunit
         (for-syntax
          syntax/parse
          syntax/stx))

(define-tokens
  NEWLINE INDENT DEDENT ENDMARKER NAME NUMBER STRING KEYWORD PUNCT)

(define-peg block
  (=> (-seq INDENT (-bind el body) DEDENT)
      el))

(define-peg body
  (=> (-bind b (-*/list el))
      `(block . ,b)))

(define-peg el
  (=> (-seq (-bind el (-or NUMBER block)) ";")
      el))


(parse block
       '((INDENT) (NUMBER 5) (PUNCT ";") (NUMBER 6) (PUNCT ";") (DEDENT)))
