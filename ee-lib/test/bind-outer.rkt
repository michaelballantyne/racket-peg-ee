#lang racket

(require "../peg.rkt"
         (for-syntax
          syntax/parse
          racket/syntax
          syntax/stx))

(define-peg test
  (-seq (-bind a (=> #\a 'a)) (=> (-bind b (=> #\b 'b)) (list a b))
        => $2))

(parse test (text "ab"))

(define-peg (helper p & v)
  (=> p (list v 'b)))

(define-peg test2
  (=> (helper (-bind a (=> #\a 'a)) & 5)
      a)) ; doesn't currently bind... I think I intended this to, at one point.

(parse test2 (text "a"))
