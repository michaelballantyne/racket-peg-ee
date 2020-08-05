#lang racket

(require "../core.rkt" (only-in "../private/forms.rkt" alt-strs))

(define-peg p1
  (alt "<" (alt ">" (alt "==" (alt ">=" (alt "<=" (alt "!=" "in")))))))

(define-peg p2
  (alt-strs "==" ">=" "<=" "<" ">" "!=" "in"))

(define times 1000000)

(time
 (for ([n (in-range times)])
   (parse p1 "in")))

(time
 (for ([n (in-range times)])
   (parse p2 "in")))
