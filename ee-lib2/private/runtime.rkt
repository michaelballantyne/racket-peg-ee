#lang racket/base

(provide (all-defined-out))

(require
  racket/performance-hint
  racket/match)

(struct failure [])
(define the-failure (failure))

(struct parse-result [index value] #:transparent)

(struct text-rep [str ix ln col] #:transparent)

(define (make-text str)
  (text-rep str 0 0 0))

(define (wrap-input in)
  (match in
    [(? string?) (make-text in)]
    [(? list?) in]
    [_ (raise-argument-error 'parse "(or/c string? list?)" in)]))

(begin-encourage-inline
  (define (fail) (values the-failure (void)))

  (define (token-pred-rt p in)
    (if (pair? in)
        (let ([res (p (car in))])
          (if res
              (values (cdr in) res)
              (fail)))
        (fail)))

  (define (step-input c ix ln col)
    (if (char=? c #\newline)
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))

  (define (string-rt s in)
    (if (and (text-rep? in)
             (<= (+ (text-rep-ix in) (string-length s)) (string-length (text-rep-str in))))
        (let loop ([ix (text-rep-ix in)]
                   [ln (text-rep-ln in)]
                   [col (text-rep-col in)]
                   [s-ix 0])
          (if (< s-ix (string-length s))
              (let ([c (string-ref (text-rep-str in) ix)])
                (if (char=? c (string-ref s s-ix))
                    (let-values ([(ix ln col) (step-input c ix ln col)])
                      (loop ix ln col (+ s-ix 1)))
                    (fail)))
              (values (text-rep (text-rep-str in) ix ln col) s)))
        (fail)))

  (define (char-pred-rt p in)
    (if (and (text-rep? in)
             (< (text-rep-ix in) (string-length (text-rep-str in))))
        (let ([c (string-ref (text-rep-str in) (text-rep-ix in))])
          (if (p c)
              (let-values
                  ([(ix ln col)
                    (step-input c (text-rep-ix in) (text-rep-ln in) (text-rep-col in))])
                (values (text-rep (text-rep-str in) ix ln col) (void)))
              (fail)))
        (fail))))