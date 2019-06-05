#lang racket

(require "peg.rkt"
         "text.rkt"
         (for-syntax syntax/parse))

(define-simple-peg-macro
  (-many-until e1 e2)
  (-local ([tmp e2])
          (-seq (-* (-seq (-! tmp) e1)) tmp)))

(define-simple-peg-macro
    (-many-until2 many-e until-e)
    (-local ([until until-e])
            (-local ([rec (-or until (-seq many-e rec))])
                    rec)))


(define-peg comment
  (-many-until2 -any-char #\newline))

(define-peg t
  (-action
   (-seq "before" (-seq (-bind c (-capture comment)) "after"))
   c))

(parse t (make-text "before; a b c\nafter"))
