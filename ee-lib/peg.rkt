#lang racket

(provide define-peg parse -eps -seq -char-pred -or -* -local -action -bind -! -debug
         define-peg-macro define-simple-peg-macro
         -char -any-char -char-range -char-except
         -string -capture -eof
         -+ -? -drop -many-until -*/list
         (rename-out [#%peg-var-with-: #%peg-var])
         (rename-out [-action =>])
         #%peg-datum
         (rename-out [make-text text])
         (struct-out parse-result)
         -token-pred)

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
   ee-lib
   syntax/parse/class/paren-shape
   (rename-in syntax/parse [define/syntax-parse def/stx])))

; Runtime

(struct failure [])
(define the-failure (failure))


(struct text [str ix ln col] #:transparent)

(define (make-text str)
  (text str 0 0 0))

(begin-encourage-inline
  (define (fail) (values the-failure (void)))
  
  (define (step-input c ix ln col)
    (if (char=? c #\newline)
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))

  (define (char-pred-rt in p)
    (if (and (text? in)
             (< (text-ix in) (string-length (text-str in))))
        (let ([c (string-ref (text-str in) (text-ix in))])
          (if (p c)
              (let-values
                  ([(ix ln col)
                    (step-input c (text-ix in) (text-ln in) (text-col in))])
                (values (text (text-str in) ix ln col) (void)))
              (fail)))
        (fail)))

  (define (string-rt in s)
    (if (and (text? in)
             (<= (+ (text-ix in) (string-length s)) (string-length (text-str in))))
        (let loop ([ix (text-ix in)]
                   [ln (text-ln in)]
                   [col (text-col in)]
                   [s-ix 0])
          (if (< s-ix (string-length s))
              (let ([c (string-ref (text-str in) ix)])
                (if (char=? c (string-ref s s-ix))
                    (let-values ([(ix ln col) (step-input c ix ln col)])
                      (loop ix ln col (+ s-ix 1)))
                    (fail)))
              (values (text (text-str in) ix ln col) s)))
        (fail)))

  (define (token-pred-rt in p)
    (if (pair? in)
        (let-values ([(match? res) (p (car in))])
          (if match?
              (values (cdr in) res)
              (fail)))
        (fail))))

; Syntactic forms and compiler

(define-literal-forms
  peg-literals
  "peg forms cannot be used as racket expressions"
  (-eps
   -char-pred
   -token-pred
   -seq2
   -or2
   -*
   -local
   -let-peg-macro
   #%peg-var
   -action
   -action/vars
   -bind
   -debug
   -string
   -capture-core
   -!))

(require (for-syntax syntax/id-table))

(begin-for-syntax
  (define-generics peg-macro
    (peg-macro-transform peg-macro stx))
  (struct peg-macro-rep (procedure)
    #:methods gen:peg-macro
    [(define (peg-macro-transform s stx)
       ((peg-macro-rep-procedure s) stx))])
  
  (define-generics parser-binding)
  (struct parser-binding-rep ()
    #:methods gen:parser-binding [])
  

  (define compiled-ids (make-free-id-table))

  (define (make-peg-rename rename-to)
    (peg-macro-rep
     (syntax-parser
       [_:id rename-to])))

  (define/hygienic (expand-peg stx) #:definition
    (syntax-parse stx
      #:literal-sets (peg-literals)
      #:datum-literals (=>)
      ; Macro
      [(~or head:id (head:id . rest))
       #:do [(define binding (lookup #'head))]
       #:when (peg-macro? binding)
       (expand-peg
        (peg-macro-transform binding stx))]
      [(~or d:char d:string)
       (with-syntax ([#%peg-datum (datum->syntax this-syntax '#%peg-datum)])
         (expand-peg (qstx/rc (#%peg-datum d))))]

      ; core form, but put first so we can assume remaining ids are references
      [-eps (values this-syntax '())]
      
      [n:id
       (with-syntax ([#%peg-var (datum->syntax this-syntax '#%peg-var)])
         (expand-peg (qstx/rc (#%peg-var n))))]
      
      ; Core forms
      [(-char-pred ~! p) (values this-syntax '())]
      [(-token-pred ~! p) (values this-syntax '())]
      [(-seq2 ~! e1 e2)
       (define-values (e1^ v1) (expand-peg #'e1))
       (define-values (e2^ v2) (expand-peg #'e2))
       (values (qstx/rc (-seq2 #,e1^ #,e2^)) (append v1 v2))]
      [(-or2 ~! e1 e2)
       (define-values (e1^ v1) (expand-peg #'e1))
       (define-values (e2^ v2) (expand-peg #'e2))
       (values (qstx/rc (-or2 #,e1^ #,e2^)) (append v1 v2))]
      [(-* ~! e)
       (define-values (e^ vs) (expand-peg #'e))
       (values (qstx/rc (-* #,e^)) '())]
      [(-local ~! [g:id e]
               b)
       (with-scope sc
         (if (identifier? #'e)
             (let ()  ; if it's a rebinding of a simple identifier, substitute and drop the -local
               (bind! (add-scope #'g sc) #'(make-peg-rename (quote-syntax e)))
               (expand-peg (add-scope #'b sc)))
             (let ()
               (def/stx g^ (bind! (add-scope #'g sc) #'(parser-binding-rep)))
               (define-values (e^ ve) (expand-peg (add-scope #'e sc)))
               (define-values (b^ vb) (expand-peg (add-scope #'b sc)))
               (define vb^ (for/list ([v vb])
                             (splice-from-scope v sc)))
               (values (qstx/rc (-local [g^ #,e^] #,b^)) vb^))))]
      [(-let-peg-macro ~! [name:id e] p)
       (with-scope sc
         (bind! (add-scope #'name sc) #'(peg-macro-rep e))
         (expand-peg (add-scope #'p sc)))]
      [(#%peg-var ~! name:id)
       (when (not (parser-binding? (lookup #'name)))
         (raise-syntax-error #f "not bound as a peg" #'name))
       (values this-syntax '())]
      [(-action ~! pe e)
       (define-values (pe^ v) (expand-peg #'pe))
       (values (qstx/rc (-action/vars #,(map syntax-local-introduce-splice v) #,pe^ e)) '())]
      [(-bind ~! x:id e)
       (define-values (e^ v) (expand-peg #'e))
       (def/stx x^ (bind! #'x #f))
       (values
        (qstx/rc (-bind x^ #,e^))
        (cons (syntax-local-introduce-splice #'x^) v))]
      [(-string ~! s:string)
       (values this-syntax '())]
      [(-capture-core ~! e)
       (define-values (e^ v) (expand-peg #'e))
       (values (qstx/rc (-capture-core #,e^)) v)]
      [(-! ~! e)
       (define-values (e^ v) (expand-peg #'e))
       (values (qstx/rc (-! #,e^)) '())]
      [(-debug ~! e)
       (define-values (e^ v) (expand-peg #'e))
       (displayln e^)
       (values e^ v)]
      [_ (raise-syntax-error #f "not a peg form" this-syntax)]))

  (define/hygienic (compile stx in) #:expression
    (syntax-parse stx
      #:literal-sets (peg-literals)
      [-eps
       #`(values #,in (void))]
      [(-seq2 e1 e2)
       (def/stx c1 (compile #'e1 in))
       (def/stx c2 (compile #'e2 #'in^))
       #'(let-values ([(in^ res) c1])
           (if (failure? in^)
               (fail)
               c2))]
      [(-or2 e1 e2)
       (def/stx c1 (compile #'e1 in))
       (def/stx c2 (compile #'e2 in))
       #'(let-values ([(in^ res) c1])
           (if (failure? in^)
               c2
               (values in^ res)))]
      [(-* e)
       (def/stx c (compile #'e #'in))
       #`(letrec ([f (lambda (in)
                       (let-values ([(in^ res^) c])
                         (if (failure? in^)
                             (values in (void))
                             (f in^))))])
           (f #,in))]
      [(-char-pred p)
       #`(char-pred-rt #,in p)]
      [(-token-pred p)
       #`(token-pred-rt #,in p)]
      [(-local [g e]
               b)
       (free-id-table-set! compiled-ids #'g (syntax-local-introduce #'f))
       (def/stx ce (compile #'e #'in^))
       (def/stx cb (compile #'b in))
       #'(letrec ([f (lambda (in^)
                       ce)])
           cb)]
      [(#%peg-var name:id)
       (def/stx f (syntax-local-introduce (free-id-table-ref compiled-ids #'name)))
       #`(f #,in)]
      [(-action/vars (v ...) pe e)
       (def/stx c (compile #'pe in))
       #'(let ([v undefined] ...)
           (let-values ([(in _) c])
             (if (failure? in)
                 (fail)
                 (let ([res e])
                   (values in res)))))]
      [(-bind x e)
       (def/stx c (compile #'e in))
       #'(let-values ([(in res) c])
           (if (failure? in)
               (fail)
               (begin
                 (set! x res)
                 (values in res))))]
      [(-string s:string)
       #`(string-rt #,in s)]
      [(-capture-core e)
       (def/stx c (compile #'e in))
       #`(if (text? #,in)
             (let-values ([(in res) c])
               (if (failure? in)
                   (fail)
                   (values in (substring (text-str #,in) (text-ix #,in) (text-ix in)))))
             (fail))]
      [(-! e)
       (def/stx c (compile #'e in))
       #`(let-values ([(in res) c])
           (if (failure? in)
               (values #,in (void))
               (fail)))]
      [_ (raise-syntax-error #f "not a core peg form" this-syntax)]
      )))

(define-syntax peg-body
  (syntax-parser
    [(_ peg-e)
     (ee-lib-boundary
      (define-values (peg-e^ vs) (expand-peg #'peg-e))
      (when (not (null? vs))
        (raise-syntax-error
         'define-peg
         "variables may only be bound within an action (=>) form"
         #f
         #f
         vs)) 
      (def/stx compiled-e (compile peg-e^ #'in))
      #'(lambda (in)
          compiled-e))]))

(define-syntax begin-for-syntax/maybe-expr
  (syntax-parser
    [(_ e)
     (if (eq? 'module (syntax-local-context))
         #'(begin-for-syntax e)
         #'(begin
             (define-syntax m (lambda (stx) e #'(begin)))
             (m))
         )]))

(define-syntax define-peg-core
  (syntax-parser
    [(_ name peg-e)
     (def/stx impl (format-id #f "~a-impl" #'name))
     #'(begin
         (define impl (peg-body peg-e))
         (define-syntax name (parser-binding-rep))
         (begin-for-syntax/maybe-expr
           ; syntax-local-introduce not necessary because this doesn't
           ; run within a macro.
           (free-id-table-set! compiled-ids #'name #'impl)))]))

(struct parse-result [index value] #:transparent)

(define-syntax parse
  (syntax-parser
    [(_ peg-name in)
     (def/stx f (syntax-local-introduce (free-id-table-ref compiled-ids #'peg-name)))
     #'(let-values ([(in^ res) (f in)])
         (if (failure? in^)
             (error 'parse "parse failed")
             (parse-result in^ res)))]))

(define-syntax-rule
  (define-peg-macro name proc)
  (define-syntax name (peg-macro-rep proc)))

(require (for-syntax syntax/parse/private/sc))

(begin-for-syntax
  (define-syntax-class head
    (pattern (name:id . rest)
             #:attr pat #'((~var name id) . rest))
    (pattern name:id
             #:attr pat #'(~var name id))))

(define-syntax (define-simple-peg-macro stx)
  (syntax-parse stx
    [(_ h:head . body)
     #`(define-peg-macro h.name
         (syntax-parser/template
          #,((make-syntax-introducer) stx)
          [h.pat . body]))]))

(define-simple-peg-macro
  (-char c:char)
  (-char-pred (lambda (v) (char=? c v))))

(define-peg-macro #%peg-datum
  (syntax-parser
    [(_ c:char) #'(-char c)]
    [(_ s:string) #'(-string s)]))

(define-simple-peg-macro
  -any-char
  (-char-pred (lambda (v) #t)))

(define-simple-peg-macro
  (-char-range c1:char c2:char)
  (-char-pred (lambda (v) (and (char>=? v c1) (char<=? v c2)))))

(define-simple-peg-macro
  (-char-except c:char ...)
  (-char-pred (lambda (v) (not (or (char=? v c) ...)))))

(define-peg-macro -seq
  (syntax-parser
    [(_ e) #'e]
    [(_ e1 e* ...)
     #'(-seq2 e1 (-seq e* ...))]))

(define-peg-macro -or
  (syntax-parser
    [(_ e) #'e]
    [(_ e1 e* ...)
     #'(-or2 e1 (-or e* ...))]))

(define-simple-peg-macro -eof (-! -any-char))

(define-simple-peg-macro (-+ e) (-local [t e] (-seq t (-* t))))

(define-peg-macro -capture
  (syntax-parser
    [(_ name:id e)
     #'(-bind name (-capture-core e))]
    [(_ e)
     #'(-capture-core e)]))

(define-peg-macro #%peg-var-with-:
  (syntax-parser
    [(_ n:id)
     (define segments (string-split (symbol->string (syntax-e #'n)) ":"))
     (match segments
       [(list _) #'(#%peg-var n)]
       [(list binder peg)
        #`(-bind #,(format-id #'n "~a" binder)
                 (#%peg-var #,(format-id #'n "~a" peg)))]
       [_ (raise-syntax-error #f "id may not have more than one `:`" #'n)])]))

(define-simple-peg-macro
  (-many-until e1 e2)
  (-seq (-* (-seq (-! e2) e1))))

(define-simple-peg-macro
  (-? e)
  (-or e -eps))

(define-simple-peg-macro
  (-*/list p)
  (-local [rec (-or (-action (-seq (-bind i p) (-bind r rec))
                             (cons i r))
                    (-action -eps
                             '()))]
          rec))

; This situation should support tail recursion, but we'd need to build it in
; to the core.
(define-peg-macro -drop
  (syntax-parser
    #:datum-literals (/)
    [(_ s ... / t)
     #'(-action (-seq s ... (-bind rest t)) rest)]))

(define-for-syntax (make-parameterized-production-transformer params body)
  (syntax-parser
    [(_ arg ...)
     #:fail-unless (= (length (syntax->list params)) (length (syntax->list #'(arg ...))))
     (format "wrong number of arguments. expected: ~a, actual: ~a"
             (length (syntax->list params)) (length (syntax->list #'(arg ...))))
     (for/fold ([body body])
               ([arg (reverse (syntax->list #'(arg ...)))]
                [param (reverse (syntax->list params))])
       #`(-local [#,param #,arg] #,body))]))
(define-for-syntax (parameterized-production-error-transformer stx)
  (raise-syntax-error #f "parameterized productions may not be called recursively" stx))

(define-syntax define-peg
  (syntax-parser
    [(_ name:id e)
     #'(define-peg-core name e)]
    [(_ (name:id param:id ...) e)
     #'(define-peg-macro name
         (make-parameterized-production-transformer
          #'(param ...)
          #'(-let-peg-macro [name parameterized-production-error-transformer]
                            e)))]))
     

; TODO:
; * parsing of token streams (hence the generic approach with `text`)
; * -capture could check there's no return val; -bind could check there is one. perf, though
; * (maybe) optimization to table-based dispatch of -or. All syntax bindings, so can inline, etc.
; * abstract over begin-for-syntax/expression pattern, perhaps at the level of the symbol table
;    pattern. Maybe my paper should discuss some of these as patterns...
; * need info about parse failures if it's to be of any real use