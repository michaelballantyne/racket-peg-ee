#lang racket/base

(provide
 check-leftrec
 (for-syntax
  expanded-defs))

(require
  "forms.rkt"
  (for-syntax
   racket/base
   syntax/id-table
   syntax/parse
   ee-lib/persistent-id-table))

(begin-for-syntax
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
       res])))

(define-syntax (check-leftrec stx)
  (define (check!)
    (for ([(k v) (in-free-id-table expanded-defs)])
      (nullable-nonterminal? k))
    (persist-free-id-table-extensions! def-nullable?))
  (if checked-leftrec
      #'(begin)
      (check!)))