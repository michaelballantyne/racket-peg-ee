#lang racket

(require "../peg.rkt")

(define-peg (f2 a b)
  (-seq (-string "a: ") a a (-string " b: ") b b))

(define-peg p (f2 (-or #\0 #\1) (-or #\x #\y)))

(parse p "a: 00 b: xx")
(parse p "a: 10 b: xy")
