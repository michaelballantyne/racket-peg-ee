#lang racket

(require "../peg.rkt")

(define-peg ab
  (-seq (-char #\a) (-char #\b)))

(define-peg ab+
  (-seq ab (-* ab)))

(define-peg ab+l
  (-* (-seq ab+ (-char #\newline))))


(define s
  (text
   (apply string-append
          (make-list 10000
                     (string-append
                      (apply string-append
                             (make-list 50 "ab"))
                      "\n")))))

(define res (parse ab+l s))


(time
 (for ([i (in-range 100)])
   (parse ab+l s)))
