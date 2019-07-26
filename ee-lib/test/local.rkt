#lang racket

(require
  "../peg.rkt"
  (for-syntax
   racket/string
   syntax/parse
   syntax/stx))

(define-peg-macro m
  (syntax-parser
    [(_ arg)
     #'(-local [bar #\b]
             (-seq arg bar))]))

(define-peg foo
  (-local [bar #\b]
          bar))

(parse foo "b")