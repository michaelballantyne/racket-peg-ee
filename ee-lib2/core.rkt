#lang racket

(provide
  eps
  seq
  alt
  *
  !
  :
  =>
  text
  token
  :src-span

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
  racket/undefined
  racket/performance-hint

  ee-lib/define
  (for-syntax
   racket/match
   racket/string
   racket/syntax
   racket/generic
   syntax/stx
   syntax/id-table
   ee-lib
   syntax/parse/class/paren-shape
   (rename-in syntax/parse [define/syntax-parse def/stx])))


; Syntactic forms

(define-literal-forms
  peg-literals
  "peg forms cannot be used as racket expressions"
  (eps
   seq
   alt
   *
   !
   :
   =>
   text
   token  ; semantics in the paper isn't enough, because it needs to support values and srclocs too.
   :src-span
   ))

; Expander
(begin-for-syntax
  (define-generics parser-binding)
  (struct parser-binding-rep ()
    #:methods gen:parser-binding [])

  (define-generics peg-macro
    (peg-macro-transform peg-macro stx))
  (struct peg-macro-rep (procedure)
    #:extra-constructor-name peg-macro
    #:methods gen:peg-macro
    [(define (peg-macro-transform s stx)
       ((peg-macro-rep-procedure s) stx))])

  (define-syntax-class peg
    #:description "PEG expression"
    (pattern e))

  (define-syntax-class nonterm-id
    #:description "PEG non-terminal name"
    (pattern n:id
      #:fail-unless
      (not (eq? unbound (lookup #'n parser-binding?)))
      "not bound as a peg"))

  ; (-> syntax? (values syntax?))
  (define/hygienic (expand-peg stx) #:definition
    (syntax-parse stx
      #:literal-sets (peg-literals)
      [eps this-syntax]
      ; Core forms
      [(seq ~! e1:peg e2:peg)
       (define-values (e1^) (expand-peg #'e1))
       (define-values (e2^) (expand-peg #'e2))
       (values (qstx/rc (seq #,e1^ #,e2^)))]
      [(alt ~! e1:peg e2:peg)
       (define-values (e1^) (expand-peg #'e1))
       (define-values (e2^) (expand-peg #'e2))
       (values (qstx/rc (alt #,e1^ #,e2^)))]
      [(* ~! e:peg)
       (define-values (e^) (expand-peg #'e))
       (values (qstx/rc (* #,e^)))]
      [(! ~! e:peg)
       (define-values (e^) (expand-peg #'e))
       (values (qstx/rc (! #,e^)))]
      [(: ~! x:id e:peg)
       (when (not (current-def-ctx))
        (raise-syntax-error
          #f
          "variables may only be bound within an action (=>) form"
          this-syntax))
       (define-values (e^) (expand-peg #'e))
       (define x^ (bind! #'x (racket-var)))
       (values (qstx/rc (: #,x^ #,e^)))]
      [(=> ~! pe:peg e:expr)
       (with-scope sc
         (define-values (pe^) (expand-peg (add-scope #'pe sc)))
         (define e^ (local-expand (add-scope #'e sc) 'expression '() (current-def-ctx)))
         (values (qstx/rc (=> #,pe^ #,e^))))]
      [(text (~or c:char s:string))
       this-syntax]
      [(token e:peg)
       (define e^ (local-expand #'e 'expression '() (current-def-ctx)))
       (values (qstx/rc (token #,e^)))]
      [(:src-span v:id ~! e:peg)
       (define-values (e^) (expand-peg #'e))
       (define v^ (bind! #'v (racket-var)))
       (values (qstx/rc (:src-span #,v^ #,e^)))]

      ; Macro
      [(~or head:id (head:id . rest))
       #:do [(define binding (lookup #'head peg-macro?))]
       #:when (not (eq? binding unbound))
       (expand-peg
        (peg-macro-transform binding stx))]

      ; Introduce #%peg-datum implicit
      [(~or d:char d:string d:number)
       (with-syntax ([#%peg-datum (datum->syntax this-syntax '#%peg-datum)])
         (expand-peg (qstx/rc (#%peg-datum d))))]

      ; Non-terminal reference
      [name:nonterm-id
       (values this-syntax)]

      [_ (raise-syntax-error #f "not a peg form" this-syntax)])))

; Compiler

(begin-for-syntax
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
      )))

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

; Runtime

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
