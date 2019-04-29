#lang racket

(require
  racket/performance-hint
  "peg.rkt"
  (submod "peg.rkt" internal)
  syntax-generic2/define
  (for-syntax
   syntax/parse
   (rename-in syntax/parse [define/syntax-parse def/stx])
   syntax-generic2))

(provide
 make-text -char -string -capture
 -char-pred -any-char -char-range #%peg-datum)

(struct text [str ix ln col])

(define (make-text str)
  (text str 0 0 0))

(begin-encourage-inline
  (define (step-input c ix ln col)
    (if (char=? c #\newline)
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))

  (define (char-pred-rt in p)
    (if (< (text-ix in) (string-length (text-str in)))
        (let ([c (string-ref (text-str in) (text-ix in))])
          (if (p c)
              (let-values
                  ([(ix ln col)
                    (step-input c (text-ix in) (text-ln in) (text-col in))])
                (values (text (text-str in) ix ln col) (void)))
              (fail)))
        (fail)))

  (define (string-rt in s)
    (if (<= (+ (text-ix in) (string-length s)) (string-length (text-str in)))
        (let loop ([ix (text-ix in)]
                   [ln (text-ln in)]
                   [col (text-col in)]
                   [s-ix 0])
          (if (< s-ix (string-length s))
              (let ([c (string-ref (text-str in) ix)])
                (if (char=? c (string-ref s s-ix))
                    (let-values ([(ix ln col) (step-input c ix ln col)])
                      (loop ix ln col (+ s-ix 1)))
                    (fail)))
              (values (text (text-str in) ix ln col) s)))
        (fail))))

(define-syntax/generics (-char-pred p)
  [(peg-expand) (values this-syntax '())]
  [(peg-compile in) #`(char-pred-rt #,in p)])

(define-syntax/generics (-string s:string)
  [(peg-expand) (values this-syntax '())]
  [(peg-compile in) #`(string-rt #,in s)])

(define-syntax/generics (-capture e)
  [(peg-expand)
   (define-values (e^ v) (dispatch-peg-expand #'e))
   (values (qstx/rc (-capture #,e^)) '())]
  [(peg-compile in)
   (def/stx c (dispatch-peg-compile #'e in))
   #`(let-values ([(in res) c])
       (values in (substring (text-str #,in) (text-ix #,in) (text-ix in))))])
    
(define-simple-peg-macro
  (-char c:char)
  (-char-pred (lambda (v) (char=? c v))))

(define-peg-macro #%peg-datum
  (syntax-parser
    [(_ c:char) #'(-char c)]
    [(_ s:string) #'(-string s)]))

(define-simple-peg-macro
  -any-char
  (-char-pred (lambda (v) #t)))

(define-simple-peg-macro
  (-char-range c1:char c2:char)
  (-char-pred (lambda (v) (and (char>=? v c1) (char<=? v c2)))))
