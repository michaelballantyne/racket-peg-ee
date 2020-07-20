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
        (fail))))