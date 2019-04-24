#lang racket

(require "peg-lines-ee.rkt")

(require (for-syntax syntax/parse))
(define-peg-macro -*2
  (syntax-parser
    [(_ e) #'(-local ([tmp2 (-or (-seq e tmp2) -eps)])
                     tmp2)]))

(define-peg ab
  (-seq (-char #\a)
        (-char #\b)))

(define-peg ab+
  (-+ ab))

(define-peg ab+l
  (-*2 (-seq ab+ (-char #\newline))))


(define s
  (apply string-append
         (make-list 10000
                    (string-append
                     (apply string-append
                            (make-list 30 "ab"))
                     "\n"))))

(parse ab+l s)

(time
 (for ([i (in-range 30)])
   (parse ab+l s)))
