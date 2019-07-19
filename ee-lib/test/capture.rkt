#lang racket
(require "../peg.rkt"
         rackunit)

(define-peg digit (-char-range #\0 #\9))
(define-peg alpha (-or (-char-range #\a #\z) (-char-range #\A #\Z)))
(define-peg p
  (-action (-seq (-bind x (-capture (-* (-or alpha digit)))) -eof)
           x))

(parse p "12a")