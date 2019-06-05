#lang racket

(require
  "peg.rkt"
  (submod "peg.rkt" internal)
  syntax-generic2/define
  (for-syntax
   syntax/parse
   (rename-in syntax/parse [define/syntax-parse def/stx])
   syntax-generic2))

(struct sequence [l])

(define-syntax/generics (-lit e)
  [(peg-expand) (values this-syntax '())]
  [(peg-compile in)
   #`(let ([l (sequence-l #,in)])
       (if (null? l)
           (fail)
           (if (equal? (car l) e)
               (values (sequence (cdr l)) (car l))
               (fail))))])

(define-syntax/generics (-list-of e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-list-of #,e^)) v)]
  [(peg-compile in)
   (def/stx c (dispatch-peg-compile #'e #'in))
   #`(let ([in (sequence #,in)])
       c)])

(define-peg l
  (-action (-list-of (-seq (-bind a (-lit "a")) (-lit "b")))
           a))

(parse l (list "a" "b"))
