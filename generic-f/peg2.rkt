#lang racket

(provide define-peg parse -eps -seq -char -or -local define-peg-macro)

(require
  racket/undefined
  racket/performance-hint
  
  syntax-generic2/define
  (for-syntax
   racket/syntax
   syntax-generic2
   (rename-in syntax/parse [define/syntax-parse def/stx])))

; Runtime

(struct failure [])
(define (fail) (values (failure) (void)))

(struct text [str ix ln col] #:transparent)

(define (make-text str)
  (text str 0 0 0))

(begin-encourage-inline
  (define (step-input c ix ln col)
    (if (char=? c #\newline)
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))

  (define (char-pred-rt in p)
    (if (< (text-ix in) (string-length (text-str in)))
        (let ([c (string-ref (text-str in) (text-ix in))])
          (if (p c)
              (let-values
                  ([(ix ln col)
                    (step-input c (text-ix in) (text-ln in) (text-col in))])
                (values (text (text-str in) ix ln col) (void)))
              (fail)))
        (fail)))

  (define (string-rt in s)
    (if (<= (+ (text-ix in) (string-length s)) (string-length (text-str in)))
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
        (fail))))

; Syntactic forms and compiler

(define-literal-forms
  peg-literals
  "peg forms cannot be used as racket expressions"
  (-eps
   -char
   -seq
   -or
   -local))

(require (for-syntax syntax/id-table))

(begin-for-syntax
  (define-syntax-generic peg-macro)
  (define-syntax-generic parser)

  (define compiled-ids (make-free-id-table))

  (define/hygienic (expand-peg stx)
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
       (define sc (make-scope))
       (def/stx g^ (bind! (add-scope #'g sc) #'(generics [parser (lambda (stx) stx)])))
       (def/stx e^ (expand-peg (add-scope #'e sc)))
       (def/stx b^ (expand-peg (add-scope #'b sc)))
       (qstx/rc
        (-local [g^ e^]
                b^))]
      [name:id
       (when (not (parser? #'name))
         (raise-syntax-error #f "not bound as a peg" #'name))
       this-syntax]
      ))

  (define/hygienic (compile stx in)
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
         (define-syntax name (generics [parser (lambda (stx) stx)]))
         (begin-for-syntax
           ; syntax-local-introduce not necessary because this doesn't
           ; run within a macro.
           (free-id-table-set! compiled-ids #'name #'runtime)))]))

(define-syntax parse
  (syntax-parser
    [(_ peg-name str-e)
     (def/stx f (syntax-local-introduce (free-id-table-ref compiled-ids #'peg-name)))
     #'(f (make-text str-e))]))

(define-syntax-rule
  (define-peg-macro name proc)
  (define-syntax name (generics [peg-macro proc])))
