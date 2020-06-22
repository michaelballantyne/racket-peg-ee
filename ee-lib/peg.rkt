#lang racket

(provide define-peg parse -eps -char-pred -or -* -local -action -bind -! -debug-expand
         define-peg-macro define-simple-peg-macro
         -char -any-char -char-range -char-except
         -string -capture -eof
         -+ -? -seq/last -many-until -*/list
         (rename-out [#%peg-var-with-: #%peg-var])
         (rename-out [-action =>])
         #%peg-datum
         (rename-out [make-text text])
         (struct-out parse-result)
         -token-pred
         -let-syntax
         -let
         (rename-out [-seq=> -seq])

         (for-syntax
          (rename-out [expand-peg local-expand-peg])
          peg-literals))

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

(struct parse-result [index value] #:transparent)

(struct text [str ix ln col] #:transparent)

(define (make-text str)
  (text str 0 0 0))

(begin-encourage-inline
  (define (fail) (values the-failure (void))))

; Syntactic forms and compiler

(define-literal-forms
  peg-literals
  "peg forms cannot be used as racket expressions"
  (-eps
   -seq2
   -or2
   -*
   -local
   #%peg-var
   -let-syntax
   -let
   -action/vars
   -bind
   -!
   -dyn

   -action ; handled by the expander but not present in fully-expanded syntax.
   ))

(module+ core
  (provide -eps -seq2 -or2 -* -local #%peg-var -let-syntax -let -action/vars
           -bind -! -dyn
           define-peg-core
           parse
           define-peg-macro
           (for-syntax
            expand-peg
            peg-literals)))

(require (for-syntax syntax/id-table))

(define-extensible-syntax peg-macro)

(begin-for-syntax
  (define-generics parser-binding)
  (struct parser-binding-rep (stx)
    #:methods gen:parser-binding [])

  (define compiled-ids (make-parameter #f))

  (define (make-peg-rename rename-to)
    (peg-macro-rep
     (syntax-parser
       [_:id rename-to])))

  (define expanded (make-parameter #f))

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
               (bind! (add-scope #'g sc) (make-peg-rename #'e))
               (expand-peg (add-scope #'b sc)))
             (let ()
               (def/stx g^ (bind! (add-scope #'g sc) (parser-binding-rep #f)))
               (define-values (e^ ve) (expand-peg (add-scope #'e sc)))
               (free-id-table-set! (expanded) #'g^ (syntax-local-introduce e^))
               (define-values (b^ vb) (expand-peg (add-scope #'b sc)))
               (define vb^ (for/list ([v vb])
                             (splice-from-scope v sc)))
               (values (qstx/rc (-local [g^ #,e^] #,b^)) vb^))))]
      [(-let-syntax ~! [name:id e] p)
       (with-scope sc
         (bind! (add-scope #'name sc) (eval-transformer #'e))
         (expand-peg (add-scope #'p sc)))]
      [(#%peg-var ~! name:id)
       (when (not (parser-binding? (lookup #'name)))
         (raise-syntax-error #f "not bound as a peg" #'name))
       (build-expanded-table! #'name)
       (values this-syntax '())]
      ; -action is handled by the core expander only to avoid the quadratic
      ; expansion time we'd get if it were a macro and the expander validated
      ; the var list with -action/vars as the only core form.
      [(~or (-action ~! pe e) (-action/vars (old-vars:id ...) pe e))
       (with-scope sc
         (define-values (pe^ v) (expand-peg (add-scope #'pe sc)))
         (def/stx e^ (local-expand (add-scope #'e sc) 'expression '() (current-def-ctx)))
         (values (qstx/rc (-action/vars #,(map syntax-local-introduce-splice v) #,pe^ e^)) '()))]
      [(-bind ~! x:id e)
       (define-values (e^ v) (expand-peg #'e))
       (def/stx x^ (bind! #'x (racket-var)))
       (values
        (qstx/rc (-bind x^ #,e^))
        (cons (syntax-local-introduce-splice #'x^) v))]
      [(-! ~! e)
       (define-values (e^ v) (expand-peg #'e))
       (values (qstx/rc (-! #,e^)) '())]
      [(-dyn f)
       (values this-syntax '())]
      [(-dyn f p)
       (define-values (p^ v) (expand-peg #'p))
       (values (qstx/rc (-dyn f #,p^)) v)]
      [(-let [v e]
             b)
       (with-scope s
         (if (identifier? #'e)
             (let ()
               (bind! (add-scope #'v s) (make-rename-transformer #'e))
               (expand-peg (add-scope #'b s)))
             (let ()
               (def/stx v^ (bind! (add-scope #'v s) (racket-var)))
               (def/stx e^ (local-expand #'e 'expression '() (current-def-ctx)))
               (define-values (b^ vs) (expand-peg (add-scope #'b s)))
               (values (qstx/rc (-let [v^ e^] #,b^)) vs))))]
      [_ (raise-syntax-error #f "not a peg form" this-syntax)]))

  (define compiled (make-parameter #f))

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
      [(-local [g e]
               b)
       (free-id-table-set! (compiled-ids) #'g (syntax-local-introduce #'f))
       (def/stx ce (compile #'e #'in^))
       (def/stx cb (compile #'b in))
       #'(letrec ([f (lambda (in^)
                       ce)])
           cb)]
      [(#%peg-var name:id)
       (build-compiled-table! #'name)
       (def/stx f (syntax-local-introduce (free-id-table-ref (compiled-ids) #'name (lambda () (error '#%peg-var/compile "no binding: ~a" #'name)))))
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
      [(-! e)
       (def/stx c (compile #'e in))
       #`(let-values ([(in res) c])
           (if (failure? in)
               (values #,in (void))
               (fail)))]
      [(-dyn f)
       #`(f #,in)]
      [(-dyn f p)
       (def/stx c (compile #'p #'in))
       #`(let ([g (lambda (in) c)])
           (f g #,in))]
      [(-let [v e] b)
       (def/stx c (compile #'b in))
       #'(let ([v e])
           c)]
      [_ (raise-syntax-error #f "not a core peg form" this-syntax)]
      )))

(define-syntax define-peg-core
  (syntax-parser
    [(_ name peg-e)
     #'(define-syntax name (parser-binding-rep #'peg-e))]))

(begin-for-syntax
  (define (free-id-table-has-key? table id)
    (if (free-id-table-ref table id (lambda () #f))
        #t
        #f))

  (define (build-expanded-table! root)
    (define table (expanded))
    (when (not (free-id-table-has-key? table root))
      (define stx (parser-binding-rep-stx (lookup root)))

      (when stx
        (free-id-table-set! table root 'tmp)

        ;(displayln `(expanding ,root))
        (define-values (expanded vars) (expand-peg (syntax-local-introduce stx)))
        ;(displayln `(done expanding ,root))
        (when (not (null? vars))
          (raise-syntax-error #f "peg definition with free binders" stx))

        (free-id-table-set! table root (syntax-local-introduce expanded)))))

  (define (build-compiled-table! root)
    (define table (compiled))

    (when (not (free-id-table-has-key? table root))
      ; local bindings will be out of context during compilation, lookup should return unbound
      (when (parser-binding-rep? (lookup root))
        (define stx (syntax-local-introduce
                     (free-id-table-ref (expanded) root)))
        (free-id-table-set! table root 'tmp)

        (define id (generate-temporary root))
        (free-id-table-set! (compiled-ids) root (syntax-local-introduce id))

        ;(displayln `(compiling ,id))
        (define compiled #`(lambda (in) #,(compile stx #'in)))
        ;(displayln `(done compiling ,id))

        (free-id-table-set! table root (syntax-local-introduce compiled))
        )))

  (define (check-left-rec! root)
    (define visited (make-free-id-table))

    (define (nullable? stx)
      (syntax-parse stx
        #:literal-sets (peg-literals)
        [-eps #t]
        [(-seq2 e1 e2)
         (and (nullable? #'e1)
              (nullable? #'e2))]
        [(-or2 e1 e2)
         (or (nullable? #'e1) (nullable? #'e2))]
        [(-* e) #t]
        [(-local [g e]
                 b)
         (nullable? #'b)]
        [(#%peg-var name:id)
         (nullable-nonterminal? #'name)]
        [(-action/vars (v ...) pe e)
         (nullable? #'pe)]
        [(-bind x e)
         (nullable? #'e)]
        [(-! e)
         (not (nullable? #'e))]
        [(-dyn f)
         #f]
        [(-dyn f p)
         (nullable? #'p)]
        [(-let [v e] b)
         (nullable? #'b)]
        ))

    (define (nullable-nonterminal? id)
      (case (free-id-table-ref visited id (lambda () 'unvisited))
        [(nullable) #t]
        [(not-nullable) #f]
        [(entered) (raise-syntax-error #f "left recursion through nonterminal" id)]
        [(unvisited)
         (free-id-table-set! visited id 'entered)
         (define res (nullable? (free-id-table-ref (expanded) id)))
         (free-id-table-set! visited id (if res 'nullable 'not-nullable))
         res]))

    (nullable-nonterminal? root))
  )

(define-syntax parse
  (syntax-parser
    [(_ peg-name in)
     (parameterize ([expanded (make-free-id-table)])
       (build-expanded-table! #'peg-name)

       (check-left-rec! #'peg-name)

       (parameterize ([compiled (make-free-id-table)]
                      [compiled-ids (make-free-id-table)])
         (build-compiled-table! #'peg-name)

         (def/stx (b ...)
           (for/list ([(k v) (in-free-id-table (compiled))])
             #`(#,(syntax-local-introduce (free-id-table-ref (compiled-ids) k))
                #,(syntax-local-introduce v))))

         #`(letrec (b ...)
             (let-values ([(in^ res) (#,(syntax-local-introduce
                                         (free-id-table-ref (compiled-ids) #'peg-name))
                                      in)])
               (if (failure? in^)
                   (error 'parse "parse failed")
                   (parse-result in^ res))))
         ))
     ]))

#|
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

(define-syntax parse
  (syntax-parser
    [(_ peg-name in)
     (def/stx f (syntax-local-introduce (free-id-table-ref compiled-ids #'peg-name)))
     #'(let-values ([(in^ res) (f in)])
         (if (failure? in^)
             (error 'parse "parse failed")
             (parse-result in^ res)))]))

|#

(begin-encourage-inline
  (define (step-input c ix ln col)
    (if (char=? c #\newline)
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))

  (define ((char-pred-rt p) in)
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

  (define ((string-rt s) in)
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

  (define ((token-pred-rt p) in)
    (if (pair? in)
        (let-values ([(match? res) (p (car in))])
          (if match?
              (values (cdr in) res)
              (fail)))
        (fail)))

  (define (capture-rt p in)
    (if (text? in)
        (let-values ([(in^ res) (p in)])
          (if (failure? in^)
              (fail)
              (values in^ (substring (text-str in) (text-ix in) (text-ix in^)))))
        (fail)))
  )

(define-simple-peg-macro
  (-char-pred p)
  (-dyn (char-pred-rt p)))

(define-simple-peg-macro
  (-string s:string)
  (-dyn (string-rt s)))

(define-simple-peg-macro
  (-token-pred p)
  (-dyn (token-pred-rt p)))

(define-simple-peg-macro
  (-capture-core p)
  (-dyn capture-rt p))

(begin-encourage-inline
  (define (char-rt c) (char-pred-rt (lambda (v) (char=? c v)))))
(define-simple-peg-macro
  (-char c:char)
  (-dyn (char-rt c)))

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
(define-peg-macro -seq/last
  (syntax-parser
    #:datum-literals (/)
    [(_ s ... t)
     #'(-action (-seq s ... (-bind rest t)) rest)]))

(define-for-syntax (make-parameterized-production-transformer peg-params rkt-params body)
  (syntax-parser
    #:datum-literals (&)
    [(_ (~seq (~peek-not &) peg-arg) ...
        (~optional (~seq & rkt-arg ...) #:defaults ([(rkt-arg 1) '()])))
     #:fail-unless (= (length (syntax->list peg-params)) (length (syntax->list #'(peg-arg ...))))
     (format "wrong number of peg arguments. expected: ~a, actual: ~a"
             (length (syntax->list peg-params)) (length (syntax->list #'(peg-arg ...))))
     #:fail-unless (= (length (syntax->list rkt-params)) (length (syntax->list #'(rkt-arg ...))))
     (format "wrong number of racket arguments. expected: ~a, actual: ~a"
             (length (syntax->list rkt-params)) (length (syntax->list #'(rkt-arg ...))))
     (define wrapped1
       (for/fold ([body body])
                 ([arg (reverse (syntax->list #'(peg-arg ...)))]
                  [param (reverse (syntax->list peg-params))])
         #`(-local [#,param #,arg] #,body)))
     (for/fold ([body wrapped1])
               ([arg (reverse (syntax->list #'(rkt-arg ...)))]
                [param (reverse (syntax->list rkt-params))])
       #`(-let [#,param #,arg] #,body))
     ]))

(define-for-syntax (parameterized-production-error-transformer stx)
  (raise-syntax-error #f "parameterized productions may not be called recursively" stx))

(define-syntax define-peg
  (syntax-parser
    #:datum-literals (&)
    [(_ name:id e)
     #'(define-peg-core name e)]
    [(_ (name:id (~seq (~peek-not &) peg-param:id) ...
                 (~optional (~seq & rkt-param:id ...) #:defaults ([(rkt-param 1) '()]))) e)
     #'(define-peg-macro name
         (make-parameterized-production-transformer
          #'(peg-param ...)
          #'(rkt-param ...)
          #'(-let-syntax [name (peg-macro-rep parameterized-production-error-transformer)]
                         e)))]))

(define-peg-macro -seq=>
  (syntax-parser
    #:literals (-action)
    [(_ p ...+ -action e)
     (def/stx ($n ...)
       (for/list ([n (in-range 1 (+ 1 (length (syntax->list #'(p ...)))))])
         (format-id this-syntax "$~a" n)))
     #'(-action (-seq (-bind $n p) ...) e)]
    [(_ p ...+)
     #'(-seq p ...)]))

(define-peg-macro -debug-expand
  (syntax-parser
    [(_ e)
     (define-values (e^ v) (expand-peg #'e))
     (displayln e^)
     e^]))


; TODO:
; * parsing of token streams (hence the generic approach with `text`)
; * -capture could check there's no return val; -bind could check there is one. perf, though
; * (maybe) optimization to table-based dispatch of -or. All syntax bindings, so can inline, etc.
; * abstract over begin-for-syntax/expression pattern, perhaps at the level of the symbol table
;    pattern. Maybe my paper should discuss some of these as patterns...
; * need info about parse failures if it's to be of any real use
