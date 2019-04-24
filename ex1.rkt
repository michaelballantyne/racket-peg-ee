#lang racket

(require "peg.rkt")

(define-peg ab
  (~seq (~char #\a)
        (~char #\b)))

(define-peg ab+
  (~seq ab (~* ab)))

(define s
  (time (apply string-append
               (make-list 100000 "ab"))))

(parse ab+ s)

(time
 (for ([i (in-range 1000)])
   (parse ab+ s)))
