#lang racket/base

(provide
 expand-peg)

(require
  ee-lib
  syntax/parse
  (for-template "forms.rkt")
  "env-reps.rkt"
  "syntax-classes.rkt")

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
    [(alt-strs s:string ...+)
     this-syntax]
    [(text (~or c:char s:string))
     this-syntax]
    [(char e:expr)
     (define e^ (local-expand #'e 'expression '() (current-def-ctx)))
     (values (qstx/rc (char #,e^)))]
    [(token e:expr)
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

    [_ (raise-syntax-error #f "not a peg form" this-syntax)]))