#lang racket

(require
  "../core.rkt"
  (for-syntax syntax/parse))

(define-syntax symbol
  (peg-macro
   (syntax-parser
     [(_ s:id)
      #'(token (lambda (t) (and (eq? t 's) 's)))])))

(define-peg t1
  (=> (seq (symbol a) (seq (: r (symbol b)) (symbol c)))
      5))

(define-peg t2
  (=> (* (: r (symbol a)))
      r))

(define-peg t3
  (=> (* (seq (symbol a) (* (: r (symbol b)))))
      r))

(define-peg t4
  (=> (alt (: a (symbol a)) (: b (symbol b)))
      (list a b)))

(define-peg t5
  (=> (* (seq (seq (! (symbol b)) (: c (token (lambda (t) t)))) eps))
      c))

(define-peg t6
  (alt (=> (symbol b) '())
       (=> (seq (: a (symbol a)) (: d t6))
           (cons a d))))

(module+ test
  (require rackunit)
  
  (check-equal?
   (parse t1 '(a b c d))
   (parse-result '(d) 5))

  (check-equal?
   (parse t2 '(a a a))
   (parse-result '() '(a a a)))

  (check-equal?
   (parse t3 '(a b a b b a b b b))
   (parse-result '() '((b) (b b) (b b b))))

  (check-equal?
   (parse t4 '(b))
   (parse-result '() '(#f b)))

  (check-equal?
   (parse t5 '(a a a b))
   (parse-result '(b) '(a a a)))

  (check-equal?
   (parse t6 '(a a a b))
   (parse-result '() '(a a a)))
  )
