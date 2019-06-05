#lang racket

(provide define-peg parse -eps -seq -char -or -local define-peg-macro)

(require peg/runtime)

(define-language core-peg "core peg grammar forms"
  -eps
  (-char c:char)
  (-seq p1:peg p2:peg)
  (-or p1:peg p2:peg)
  (-local [n:id rhs:peg]
          body:peg)
  (-nonterm-ref n:id))

(define-binding-type peg-macro)
(define-binding-type peg-parser)

(define-expander (expand-peg stx)
  [c:char (expand-peg #'(-char c))]
  [n:id (expand-peg #'(-nonterm-ref n))]
  [(head . rest)
   #:when (peg-macro? stx)
   (expand-peg (peg-macro stx))]
  
  #:language core-peg
  [-eps ]
  [-char ]
  [-or ]
  [-local ]
  [-nonterm-ref ]
  )
  
  

(begin-for-syntax
  (struct parser-info [])

  (define-syntax-generic peg-macro)

  (define compiled-ids (make-free-id-table))

  (define (expand-peg stx [ctx #f])
    (apply-as-transformer expand-peg-internal 'expression ctx stx))
  (define (expand-peg-internal stx)
    (syntax-parse stx
      #:literal-sets (peg-literals)
      [c:char (expand-peg #'(-char c))]

      ; Macro
      [(head . rest)
       #:when (peg-macro? stx)
       (expand-peg
        (peg-macro stx))]
      
      ; Core forms
      [-eps this-syntax]
      [(-char c:char) this-syntax]
      [(-seq e1 e2)
       (def/stx e1^ (expand-peg #'e1))
       (def/stx e2^ (expand-peg #'e2))
       (qstx/rc (-seq e1^ e2^))]
      [(-or e1 e2)
       (def/stx e1^ (expand-peg #'e1))
       (def/stx e2^ (expand-peg #'e2))
       (qstx/rc (-or e1^ e2^))]
      [(-local [g e]
               b)
       (define ctx (make-def-ctx))
       (define sc (make-scope))
       (def/stx g^ (bind! ctx (add-scope #'g sc) #'(parser-info)))
       (def/stx e^ (expand-peg (add-scope #'e sc) ctx))
       (def/stx b^ (expand-peg (add-scope #'b sc) ctx))
       (qstx/rc
        (-local [g^ e^]
                b^))]
      [name:id
       (define env-val (syntax-local-value #'name (lambda () #f)))
       (when (or (not env-val) (not (parser-info? env-val)))
         (raise-syntax-error #f "not bound as a peg" #'name))
       this-syntax]
      ))

  (define (compile stx in)
    (apply-as-transformer compile-internal 'expression #f stx in))
  (define (compile-internal stx in)
    (syntax-parse stx
      #:literal-sets (peg-literals)
      [-eps
       #`(values #,in (void))]
      [(-seq e1 e2)
       (def/stx c1 (compile #'e1 in))
       (def/stx c2 (compile #'e2 #'in^))
       #'(let-values ([(in^ res) c1])
           (if (failure? in^)
               (fail)
               c2))]
      [(-or e1 e2)
       (def/stx c1 (compile #'e1 in))
       (def/stx c2 (compile #'e2 in))
       #'(let-values ([(in^ res) c1])
           (if (failure? in^)
               c2
               (values in^ res)))]
      [(-char c:char)
       #`(char-pred-rt #,in (lambda (v) (eqv? c v)))]
      [(-local [g e]
               b)
       (free-id-table-set! compiled-ids #'g (syntax-local-introduce #'f))
       (def/stx ce (compile #'e #'in^))
       (def/stx cb (compile #'b in))
       #'(letrec ([f (lambda (in^)
                       ce)])
           cb)]
      [name:id
       (def/stx f (syntax-local-introduce (free-id-table-ref compiled-ids #'name)))
       #`(f #,in)]
      )))

(define-syntax peg-body
  (syntax-parser
    [(_ peg-e)
     (define expanded-e (expand-peg #'peg-e))
     (def/stx compiled-e (compile expanded-e #'in))
     #'(lambda (in)
         compiled-e)]))

(define-syntax define-peg
  (syntax-parser
    [(_ name peg-e)
     #'(begin
         (define runtime (peg-body peg-e))
         (define-syntax name (parser-info))
         (begin-for-syntax
           (free-id-table-set! compiled-ids #'name #'runtime)))]))

(define-syntax parse
  (syntax-parser
    [(_ peg-name str-e)
     (def/stx f (syntax-local-introduce (free-id-table-ref compiled-ids #'peg-name)))
     #'(f (make-text str-e))]))

(define-syntax-rule
  (define-peg-macro name proc)
  (define-syntax name (generics [peg-macro proc])))
