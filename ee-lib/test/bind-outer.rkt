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
  (-debug-expand (-left [a (=> #\a 'a)] (helper #\b & (list a)))))

(parse test2 (text "ab"))