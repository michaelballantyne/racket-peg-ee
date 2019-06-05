#lang racket

(require
  (for-syntax syntax/parse)
  (rename-in "peg2.rkt" [-seq -seq2]))

#;(define-peg-macro -seq
    (syntax-parser
      [(_) #'-eps]
      [(_ e) #'e]
      [(_ e1 e2)
       #'(-seq2 e1 e2)]
      [(_ e1 e2 e3 ...+)
       #'(-seq2 e1 (-seq e2 e3 ...))]))

#;(define-peg g (-char #\b))
#;(define-peg ab (-seq2 (-char #\a) g))

(define-peg ab
  (-seq2
   (-local [g (-char #\b)]
           (-seq2 (-char #\a) g))
   (-char #\c)))

(parse ab "abc")