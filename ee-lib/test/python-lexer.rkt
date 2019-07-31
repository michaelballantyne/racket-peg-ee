#lang racket

(require "../peg.rkt"
         rackunit
         racket/performance-hint
         (for-syntax
          racket/string
          syntax/parse
          syntax/stx))

(provide lexer)


(define (compute-dedent new-level indent-stack)
  (let loop ([s indent-stack]
             [count 0])
    (cond
      [(< new-level (first s))
       (loop (rest s) (+ 1 count))]
      [(= new-level (first s))
       (values s count)]
      [(> new-level (first s))
       (error 'lexer "inconsistent indention")])))

(define (make-offsides-handler)
  (define indent-stack '(0))
  (define (current-indent) (first indent-stack))

  (define (line-indent! spaces)
    (cond
      [(> spaces (current-indent))
       (set! indent-stack (cons spaces indent-stack))
       '((INDENT))]
      [(= spaces (current-indent))
       '()]
      [(< spaces (current-indent))
       (define-values (new-stack dedent-count)
         (compute-dedent spaces indent-stack))
       (set! indent-stack new-stack)
       (make-list dedent-count '(DEDENT))]))
  line-indent!)
  
  
(define (lexer str)
  (define line-indent! (make-offsides-handler))
  
  (define (eof-dedent!)
    (append (line-indent! 0) '((ENDMARKER))))

  ; TODO: actual unicode!
  (define-peg Lu (-char-range #\A #\Z))
  (define-peg Ll (-char-range #\a #\z))
  (define-peg Nd (-char-range #\0 #\9))
  
  (define-peg id-start (-or Lu Ll))
  (define-peg id-continue (-or id-start Nd))
  (define-peg identifier
    (=> (-capture s (-seq id-start (-* id-continue)))
        `(NAME ,s)))

  (define-peg-macro -keywords
    (syntax-parser
      [(_ ss)
       #:with (s ...) (map (lambda (s) (datum->syntax #'here s))
                           (string-split (syntax-e #'ss)))
       #'(-or (-string s) ...)]))
  
  (define-peg keyword
    (=> (-capture
         s
         (-keywords
          "False      await      else       import     pass
          None       break      except     in         raise
          True       class      finally    is         return
          and        continue   for        lambda     try
          as         def        from       nonlocal   while
          assert     del        global     not        with
          async      elif       if         or         yield"))
        `(KEYWORD ,s)))

  (define-peg operator
    (-keywords
     "+       -       *       **      /       //      %      @
      <<      >>      &       |       ^       ~
      <       >       <=      >=      ==      !="))

  (define-peg delimiter
    (-keywords
     "(       )       [       ]       {       }
      ,       :       .       ;       @       =       ->
      +=      -=      *=      /=      //=     %=      @=
      &=      |=      ^=      >>=     <<=     **="))

  (define-peg punctuation
    (=> (-capture s (-or operator delimiter))
        `(PUNCT ,s)))

  (define-peg nonzerodigit (-char-range #\1 #\9))
  (define-peg digit (-char-range #\0 #\9))
  
  (define-peg decinteger
    (=> (-capture s (-or (-seq nonzerodigit (-* (-seq (-? #\_) digit)))
                         (-seq (-+ #\0) (-* (-seq (-? #\_) #\0)))))
        `(NUMBER
          ,(string->number
            (list->string
             (filter (lambda (c) (not (char=? c #\_))) (string->list s)))))))

  (define-peg integer (-or decinteger))

  (define-peg string-prefix
    (-keywords "r u R U f F fr Fr fR Fr rf rF Rf RF")) ; TODO: meaning
  (define-peg bytes-prefix
    (-keywords "b B br Br bR BR rb rB Rb RB"))
  
  (define-peg -ascii-char (-char-pred (lambda (c) (< (char->integer c) 128))))
  
  (define-peg string-escape-seq
    (=> (-seq #\\ -any-char)
        (error 'parse "escape sequences not supported yet"))) ; TODO

  (define-peg (string-variant qt non-char char escape)
    (-local [item (-or escape (-capture (-seq (-! non-char) char)))]
            (=> (-seq qt (-bind chars (-*/list item)) qt)
                chars)))
  (define-peg (string-variants char esc)
    (-or (string-variant #\' (-or #\\ #\newline #\') char esc)
         (string-variant #\" (-or #\\ #\newline #\") char esc)
         (string-variant (-seq #\' #\' #\') (-or #\\ (-seq #\' #\' #\')) char esc)
         (string-variant (-seq #\" #\" #\") (-or #\\ (-seq #\" #\" #\")) char esc)))

  (define-peg string
    (=> (-seq
         #; (-? string-prefix)  ; TODO
         (-bind chars (-debug (string-variants -any-char string-escape-seq))))
        `(STRING ,(apply string-append chars))))

  (define-peg literal (-or integer string)) ; TODO: bytes, other numbers

  (define-peg simple-token (-or keyword identifier literal punctuation))

  ; TODO: tabs
  (define-peg whitespace-char (-or #\space))
  (define-peg whitespace (-* whitespace-char))

  (define-peg indent
    (=> (-capture s whitespace)
        ; TODO: Side-effect. Should I support effect backtracking?
        (line-indent! (string-length s))))
  
  ; TODO: other line termination sequences
  (define-peg line-terminator (-or #\newline))
  
  (define-peg comment (-seq #\# (-many-until -any-char #\newline)))

  (define-peg blank-line (-seq whitespace (-? comment)))
  
  (define-peg line-start
    (-or
     (=> (-seq blank-line -eof)                      ; EOF with dedents
         (eof-dedent!))
     
     (-drop blank-line line-terminator / line-start) ; blank line
     
     (=> (-seq i:indent rest:line-continue)          ; normal line
         (append i rest))))

  (define-peg line-continue
    (-or
     (=> -eof                                        ; EOF with dedents
         (cons '(NEWLINE) (eof-dedent!)))
     (=> (-seq line-terminator rest:line-start)      ; newline
         (cons '(NEWLINE) rest))
     
     (-drop comment / line-continue)                 ; comment
     (-drop #\\ line-terminator / line-continue)     ; line continuation
     (-drop whitespace-char / line-continue)         ; between-token whitespace

     (=> (-seq t:simple-token rest:line-continue)    ; simple tokens
         (cons t rest))))

  (parse line-start (text str)))

(module+ test

  (lexer
   #<<here
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
here
   )

  (time
   (for ([i (in-range 1 1000)])
     (lexer
      #<<here
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
# Comment 1
 foo
  bar baz
 baz False
 # foo
'foo' if 5 else True
1_1 + 3,
here
   
      )
     )))