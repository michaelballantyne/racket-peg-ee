#lang racket

(require (except-in "../peg.rkt" #%peg-datum)
         "../token.rkt"
         "python-lexer.rkt"
         rackunit
         (for-syntax
          syntax/parse
          racket/syntax
          syntax/stx))

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

(define-peg test
  (-seq/last e:or-test
             (-or (-seq "if" or-test "else" test => `(IfExpr ,$2 ,e ,$4))
                  (=> -eps e))))

(define-peg test-nocond
  (-or or-test))

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
   (-+ STRING)
   (-seq "{" dict-or-set-maker "}" => $2)))

(define-peg (comma-list el)
  (-seq (-*/list (-seq "," el => $2)) (-? ",") => $1))

(define-peg star-expr
  (-seq "*" expr => `(StarExpr ,$2)))

(define-peg star-star-expr
  (-seq "**" expr => `(StarStarExpr ,$2)))

(define-peg (set-literal & head)
  (=> (-bind l (comma-list (-or test star-expr)))
      `(Set ,head ,@l)))

(define-peg (dict-literal & head)
  (=> (-bind l (comma-list (-or (-seq test ":" test => (list $1 $3)) star-star-expr)))
      `(Dict ,head ,@l)))

(define-peg dict-or-set-maker
  (-or
   (-seq/last s:star-expr (-or (set-literal & s) #;(comp-for s)))
   (-seq/last s:star-star-expr (-or (dict-literal & s) #;(comp-for s)))
   (-seq/last t1:test
              (-or (-seq/last ":" t2:test (-or (dict-literal & (list t1 t2))
                                               #;(comp-for t1 t2)))
                   #;(comp-for t1)
                   (set-literal & t1)))))

(define-peg comp-for
  (-seq "for" expr-list "in" or-test (-? comp-iter)))

(define-peg comp-iter
  (-or comp-for
       comp-if))

(define-peg comp-if
  (-seq "if" test-nocond (-? comp-iter)))

(define-peg expr-list
  (comma-list (-or expr star-expr)))


(define (parser tokens)
  (parse or-test tokens))


(module+ test
  (require rackunit)
  
  (define (lex+parse s) (parse-result-value (parser (parse-result-value (lexer s)))))

  (check-equal?
   (lex+parse
    "x or not 1 * 1 | 2 + 2 * 3 is not 5")
   `(BinOp
     "x"
     "or"
     (UnaryOp
      "not"
      (BinOp
       (BinOp
        (BinOp
         1
         "*"
         1)
        "|"
        (BinOp
         2
         "+"
         (BinOp
          2
          "*"
          3)))
       "is not"
       5))))

  (check-equal?
     (lex+parse
      "{x, y}")
     '(Set "x" "y")))