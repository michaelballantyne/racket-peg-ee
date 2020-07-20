#lang racket

(provide
 (all-from-out "private/forms.rkt")

 define-peg
 parse
 parse-result

 #%peg-datum

 (for-syntax
  (rename-out [expand-peg local-expand-peg])
  peg-macro
  gen:peg-macro
  peg-macro?
  peg-macro/c
  peg-macro-transform))

(require
  "private/forms.rkt"
  "private/leftrec-check.rkt"
  "private/runtime.rkt"
  (for-syntax
   syntax/parse
   racket/syntax
   syntax/id-table
   (rename-in syntax/parse [define/syntax-parse def/stx])
   "private/env-reps.rkt"
   "private/syntax-classes.rkt"
   "private/expand.rkt"
   "private/compile.rkt"))

; Interface macros

(define-syntax define-peg
  (syntax-parser
    [(_ name:id peg-e:peg)
     (when (not (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "define-peg only works in module context" this-syntax))
     (def/stx impl (generate-temporary #'name))
     (syntax-local-lift-module-end-declaration
      #'(define-peg-pass2 name peg-e impl))
     #'(begin
         (begin-for-syntax
           (free-id-table-set! compiled-ids #'name #'impl))
         (define-syntax name (parser-binding-rep)))]))

(define-syntax define-peg-pass2
  (syntax-parser
    [(_ name peg-e impl)
     (define-values (peg-e^) (expand-peg #'peg-e))
     (free-id-table-set! expanded-defs (syntax-local-introduce #'name) (syntax-local-introduce peg-e^))
     (syntax-local-lift-module-end-declaration
      #'(check-leftrec))
     (syntax-local-lift-module-end-declaration
      #`(define-peg-pass3 name #,peg-e^ impl))
     #'(begin)]))

(define-syntax define-peg-pass3
  (syntax-parser
    [(_ name peg-e impl)
     (define compiled-e (compile-peg #'peg-e #'in))
     #`(define impl (lambda (in) #,compiled-e))]))


(define-syntax parse
  (syntax-parser
    [(_ peg-name:nonterm-id in-e:expr)
     (def/stx f (syntax-local-introduce (free-id-table-ref compiled-ids #'peg-name)))
     #'(let ([in (wrap-input in-e)])
         (let-values ([(in^ res) (f in)])
           (if (failure? in^)
               (error 'parse "parse failed")
               (parse-result in^ res))))]))

; Default implementation of #%peg-datum interposition point

(define-syntax #%peg-datum
  (peg-macro
   (syntax-parser
     [(_ (~or* v:char v:string)) #'(text v)])))