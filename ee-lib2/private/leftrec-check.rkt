#lang racket/base

(provide
 lift-leftrec-check!)

(require
  syntax/id-table
  syntax/parse
  ee-lib/persistent-id-table

  (for-template
   "forms.rkt"))

(define expanded-defs (make-free-id-table))
(define checked-leftrec #f)
(define-persistent-free-id-table def-nullable?)

(define (nullable? stx)
  (syntax-parse stx
    #:literal-sets (peg-literals)
    [eps #t]
    [(seq e1 e2)
     (and (nullable? #'e1)
          (nullable? #'e2))]
    [(alt e1 e2)
     (or (nullable? #'e1)
         (nullable? #'e2))]
    [(* e) #t]
    [(! e)
     (not (nullable? #'e))]
    [(: x e)
     (nullable? #'e)]
    [(=> pe e)
     (nullable? #'pe)]
    [(text t) #f]
    [(token f) #f]
    [(char f) #f]
    [name:id
     (nullable-nonterminal? #'name)]
    [(:src-span v e)
     (nullable? #'e)]
    [_ (raise-syntax-error #f "not a core peg form" this-syntax)]))

(define (nullable-nonterminal? id)
  (case (persistent-free-id-table-ref def-nullable? id (lambda () 'unvisited))
    [(nullable) #t]
    [(not-nullable) #f]
    [(entered) (raise-syntax-error #f "left recursion through nonterminal" id)]
    [(unvisited)
     (persistent-free-id-table-set! def-nullable? id 'entered)
     (define res (nullable? (free-id-table-ref expanded-defs id)))
     (persistent-free-id-table-set! def-nullable? id (if res 'nullable 'not-nullable))
     res]))

(define (check-leftrec)
  (define (check!)
    (for ([(k v) (in-free-id-table expanded-defs)])
      (nullable-nonterminal? k))
    (persist-free-id-table-extensions! def-nullable?))
  (if checked-leftrec
      #'(begin)
      (check!)))


(module apply-for-syntax racket/base
  (require (for-syntax racket/base syntax/parse racket/syntax))
  (provide apply-for-syntax)
  (define-syntax apply-for-syntax
    (syntax-parser
      [(_ id)
       ((syntax-local-eval #'id))])))
(require (for-template 'apply-for-syntax))

(define (lift-leftrec-check! name rhs)
  (free-id-table-set!
   expanded-defs
   (syntax-local-introduce name)
   (syntax-local-introduce rhs))
  
  (syntax-local-lift-module-end-declaration
   #'(apply-for-syntax check-leftrec)))
