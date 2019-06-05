#lang racket

(require "peg2.rkt")

(define-peg a
  (-or (-seq (-char #\a) b)
       -eps))

(define-peg b
  (-or (-seq (-char #\b) a)
       -eps))

(parse a "abab")