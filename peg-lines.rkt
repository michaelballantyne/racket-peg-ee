#lang racket

(require (for-syntax racket/syntax))
(require racket/performance-hint)

(provide define-peg parse -eps -char -or -seq -* -!)

(define-syntax -eps #f)
(define-syntax -char #f)
(define-syntax -or #f)
(define-syntax -seq #f)
(define-syntax -* #f)
(define-syntax -! #f)

(begin-encourage-inline
  (define (advance-input-1 c ix ln col)
    (if c
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))
  
  (define (char-rt in ix ln col c)
    (if (< ix (string-length in))
        (let ([c2 (string-ref in ix)])
          (if (char=? c2 c)
              (advance-input-1 c2 ix ln col)
              (values #f ln col)))
        (values #f ln col))))

(begin-for-syntax
  (struct parser [f])
  
  (define (compile e in ix ln col)
    (with-syntax ([in in] [ix ix] [ln ln] [col col])
      (syntax-case e (-eps -char -or -seq -* -!)
        [-eps
         #'(values ix ln col)]
        [(-char c)
         #'(char-rt in ix ln col c)]
        [(-or e1 e2)
         (with-syntax ([c1 (compile #'e1 #'in #'ix #'ln #'col)]
                       [c2 (compile #'e2 #'in #'ix #'ln #'col)])
           
           #'(let-values ([(ix ln col) c1])
               (if ix
                   (values ix ln col)
                   c2)))]
        [(-seq e1 e2)
         (with-syntax ([c1 (compile #'e1 #'in #'ix #'ln #'col)]
                       [c2 (compile #'e2 #'in #'ix #'ln #'col)])
           #'(let-values ([(ix ln col) c1])
               (if ix
                   c2
                   (values ix ln col))))]
        ; can't encode as [r (or (seq e r) eps)] due to loss of tail recursion from `or`
        [(-* e)
         (with-syntax ([c (compile #'e #'in #'ix #'ln #'col)])
           #'(letrec ([f (lambda (ix ln col)
                           (let-values ([(ix^ ln^ col^) c])
                             (if ix^
                                 (f ix^ ln^ col^)
                                 (values ix ln col))))])
               (f ix ln col)))]
        ; TODO: test
        #;[(~! e)
           (with-syntax ([c (compile #'e #'in #'ix #'ln #'col)])
             #'(if c ix #f))]
        [nonterm
         (identifier? #'nonterm)
         (with-syntax ([f (syntax-local-introduce (parser-f (syntax-local-value #'nonterm)))])
           #'(f in ix ln col))]
        ))))

(define-syntax (define-peg stx)
  (syntax-case stx ()
    [(_ name e)
     (with-syntax ([f (generate-temporary #'name)])
       (with-syntax ([body (compile #'e #'in #'ix #'ln #'col)])
         #'(begin
             (define (f in ix ln col) body)
             (define-syntax name (parser #'f)))))]))

(define-syntax (parse stx)
  (syntax-case stx ()
    [(_ nonterm str-e)
     (with-syntax ([f (parser-f (syntax-local-value #'nonterm))])
       #'(f str-e 0 0 0))]))

