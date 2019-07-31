#lang racket

(require (rename-in (except-in "../peg.rkt" #%peg-datum) [-seq -seq-core])
         "../token.rkt"
         "python-lexer.rkt"
         rackunit
         (for-syntax
          syntax/parse
          racket/syntax
          syntax/stx))

(define-peg-macro -seq
  (syntax-parser
    #:literals (=>)
    [(_ p ... => e)
     (define/syntax-parse ($n ...)
       (for/list ([n (in-range 1 (+ 1 (length (syntax->list #'(p ...)))))])
         (format-id this-syntax "$~a" n)))
     #'(=> (-seq-core (-bind $n p) ...) e)]
    [(_ p ...)
     #'(-seq-core p ...)]))

(define-peg-macro -seq/list
  (syntax-parser
    [(_ p ...)
     (define/syntax-parse (n ...) (generate-temporaries #'(p ...)))
     #'(=> (-seq (-bind n p) ...)
           (list n ...))]))

(define-tokens
  NEWLINE INDENT DEDENT ENDMARKER NAME STRING NUMBER KEYWORD PUNCT)

(define-peg-macro nest-right
  (syntax-parser
    [(_ last) #'last]
    [(_  (init ...) rest ...)
     #'(init ... (nest-right rest ...))]))

(define (associate-binops e1 op+e*)
  (for/fold ([base e1]) ([op+e op+e*])
    (match op+e
      [(list op e) `(BinOp ,base ,op ,e)])))

(define-peg (nary op expr)
  (-seq expr (-*/list (-seq/list op expr))
        => (associate-binops $1 $2)))

(define (associate-prefix-ops ops expr)
  (match ops
    ['() expr]
    [(cons op rest) `(UnaryOp ,op ,(associate-prefix-ops rest expr))]))

(define-peg (prefix op expr)
  (-seq (-*/list op) expr
        => (associate-prefix-ops $1 $2)))

(define-peg or-test
  (nest-right
   (nary "or")
   (nary "and")
   (prefix "not")
   (nary (-or "<" ">" "==" ">=" "<=" "<>" "!="
              "in" (=> (-seq "not" "in") "not in")
              (=> (-seq "is" "not") "is not") "is"))
   expr))

(define-peg expr
  (nest-right
   (nary "|")
   (nary "^")
   (nary "&")
   (nary (-or "<<" ">>"))
   (nary (-or "+" "-"))
   (nary (-or "*" "@" "/" "%" "//"))
   factor))

(define-peg factor
  (-or atom))

(define-peg atom
  (-or
   NAME
   NUMBER
   (-+ STRING)))


(define (parser tokens)
  (parse or-test tokens))


(module+ test
  (define tokens
    (parse-result-value
     (lexer
      "x or not 1 * 1 | 2 + 2 * 3 is not 5")))

  (parser
   tokens))
