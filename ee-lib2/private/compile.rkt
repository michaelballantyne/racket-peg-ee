#lang racket/base

(provide
 compile-peg
 compiled-ids)

(require
  syntax/parse
  (rename-in syntax/parse [define/syntax-parse def/stx])
  syntax/id-table
  ee-lib
  (for-template (except-in racket/base => *))
  (for-template "forms.rkt")
  (for-template "runtime.rkt"))

(define compiled-ids (make-free-id-table))

(define (bound-vars e)
  (syntax-parse e
    #:literal-sets (peg-literals)
    [(: v rhs)
     (list #'v)]
    [(seq e1 e2)
     (append (bound-vars #'e1) (bound-vars #'e2))]
    [(alt e1 e2)
     (append (bound-vars #'e1) (bound-vars #'e2))]
    [(* e)
     (bound-vars #'e)]
    [(:src-span v e)
     (bound-vars #'e)]
    [_ '()]))

(define v-tmps (make-parameter #f))

(define/hygienic (compile-peg stx in) #:expression
  (syntax-parse stx
    #:literal-sets (peg-literals)
    [eps
     #`(values #,in (void))]
    [(seq e1 e2)
     (def/stx c1 (compile-peg #'e1 in))
     (def/stx c2 (compile-peg #'e2 #'in^))
     #'(let-values ([(in^ res) c1])
         (if (failure? in^)
             (fail)
             c2))]
    [(alt e1 e2)
     (def/stx c1 (compile-peg #'e1 in))
     (def/stx c2 (compile-peg #'e2 in))
     #'(let-values ([(in^ res) c1])
         (if (failure? in^)
             c2
             (values in^ res)))]
    [(* e)
     (def/stx (v* ...) (bound-vars #'e))
     (def/stx (outer-v* ...) (for/list ([v (attribute v*)])
                               (syntax-local-introduce (free-id-table-ref (v-tmps) v))))
     (def/stx (iter-v* ...) (generate-temporaries (attribute v*)))
     (def/stx (inner-v* ...) (generate-temporaries (attribute v*)))
     (def/stx c
       (parameterize ([v-tmps (for/fold ([t (v-tmps)])
                                        ([orig-v (attribute v*)]
                                         [inner-v (attribute inner-v*)])
                                (free-id-table-set t orig-v (syntax-local-introduce inner-v)))])
         (compile-peg #'e #'in)))
     #`(let ([iter-v* '()] ...)
         (letrec ([f (lambda (in)
                       (let ([inner-v* #f] ...)
                         (let-values ([(in^ res^) c])
                           (if (failure? in^)
                               (begin
                                 (set! outer-v* (reverse iter-v*)) ...
                                 (values in (void)))
                               (begin
                                 (set! iter-v* (cons inner-v* iter-v*)) ...
                                 (f in^))))))])
           (f #,in)))]
    [(! e)
     (def/stx c (compile-peg #'e in))
     #`(let-values ([(in res) c])
         (if (failure? in)
             (values #,in (void))
             (fail)))]
    ; TODO string capture when text
    [(: x e)
     (def/stx c (compile-peg #'e in))
     #`(let-values ([(in res) c])
         (if (failure? in)
             (fail)
             (begin
               (set! #,(syntax-local-introduce (free-id-table-ref (v-tmps) #'x)) res)
               (values in (void)))))]
    [(=> pe e) ; TODO variable lifting
     (def/stx (v* ...) (bound-vars #'pe))
     (def/stx c
       (parameterize ([v-tmps (for/fold ([t (make-immutable-free-id-table)])
                                        ([v (attribute v*)])
                                (free-id-table-set t v (syntax-local-introduce v)))])
         (compile-peg #'pe in)))
     #'(let ([v* #f] ...)
         (let-values ([(in _) c])
           (if (failure? in)
               (fail)
               (let ([res e])
                 (values in res)))))]
    [(text t)
     (error 'compile-peg "text not yet supported")]
    [(token f) ; TODO probably needs a contract check
     #`(token-pred-rt f #,in)]
    [name:id
     (def/stx f (syntax-local-introduce
                 (free-id-table-ref
                  compiled-ids #'name
                  (lambda () (error 'compile-peg "no compiled id for: ~a" #'name)))))
     #`(f #,in)]
    [(:src-span v e)
     (error 'compile-peg ":src-span not yet supported")]
    [_ (raise-syntax-error #f "not a core peg form" this-syntax)]
    ))