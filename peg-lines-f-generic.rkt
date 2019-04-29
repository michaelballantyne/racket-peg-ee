#lang racket

(require (for-syntax racket/syntax))
(require racket/performance-hint)

(provide define-peg parse -eps -char -or -seq -* -!)

(struct st [in ix ln col] #:prefab)

(begin-encourage-inline
  (define-inline (advance-input-1 c s)
    (if (char=? c #\newline)
        (st (st-in s)
            (+ (st-ix s) 1)
            (+ (st-ln s) 1)
            0)
        (st (st-in s)
            (+ (st-ix s) 1)
            (st-ln s)
            (+ (st-col s) 1))))

  (define -eps
    (lambda (s) s))

  (define-inline (-char c)
    (lambda (s)
      (if (< (st-ix s) (string-length (st-in s)))
          (let ([c2 (string-ref (st-in s) (st-ix s))])
            (if (char=? c2 c)
                (advance-input-1 c2 s)
                #f))
          #f)))

  (define-inline (-or c1 c2)
    (lambda (s)
      (let ([s^ (c1 s)])
        (if s^
            s^
            (c2 s)))))

  (define-inline (-seq c1 c2)
    (lambda (s)
      (let ([s^ (c1 s)])
        (if s^
            (c2 s^)
            #f))))

  (define-inline (-* c)
    (lambda (s)
      (letrec ([f (lambda (s)
                    (let ([s^ (c s)])
                      (if s^
                          (f s^)
                          s)))])
        (f s))))

  (define-inline (-! c)
    (lambda (s)
      (let ([s^ (c s)])
        (if s^
            #f
            s))))

  )


(define-syntax (define-peg stx)
  (syntax-case stx ()
    [(_ name e)
     #'(define name e)]))

(define-syntax (parse stx)
  (syntax-case stx ()
    [(_ nonterm str-e)
     #'(nonterm (st str-e 0 0 0))]))

