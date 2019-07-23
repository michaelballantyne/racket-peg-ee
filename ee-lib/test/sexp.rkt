#lang racket

(require "../peg.rkt"
         rackunit)

(define-peg digit (-char-range #\0 #\9))
(define-peg alpha (-or (-char-range #\a #\z) (-char-range #\A #\Z)))
(define-peg idchar (-or alpha #\+ #\- #\* #\% #\= #\! #\< #\> #\- #\\ #\? #\_))

(define-peg id
  (=> (-capture s (-seq idchar (-* (-or digit idchar))))
      (string->symbol s)))

(define-peg whitespace
  (-+ (-or #\space #\newline)))

(define-peg integer
  (=> (-capture s (-or #\0 (-seq (-char-range #\1 #\9) (-* digit))))
      (string->number s)))

(define-peg string
  (=> (-seq #\" (-capture s (-many-until -any-char #\")) #\")
      s))

(define-peg keyword
  (=> (-seq #\: (-capture s (-+ (-or digit idchar))))
      (string->keyword s)))

(define-peg comment
  (-seq #\; (-many-until -any-char #\newline)))

(define-peg sexp
  (-or
   id
   integer
   string
   keyword
   (=> (-seq #\( s:sexp-list #\)) s)
   (=> (-seq #\[ s:sexp-list #\]) s)))

(define-peg empty-as-list (=> -eps '()))

(define-peg sexp-list
  (-or
   (=> (-seq whitespace s:sexp-list) s)
   (=> (-seq comment s:sexp-list) s)
   (=> (-seq a:sexp (-bind d (-or (-seq whitespace sexp-list) empty-as-list)))
       (cons a d))
   empty-as-list))

(parse sexp
       #<<end
(x :y 1 "a" (b) ; comment
)
end
       )