#lang racket

(require "../peg.rkt"
         rackunit
         (for-syntax
          syntax/parse
          syntax/stx))

(define (lexer str)
  (define indent-stack '(0))
  (define (current-indent) (first indent-stack))
  (define (indent! new-level) (set! indent-stack (cons new-level indent-stack)) '((INDENT)))
  (define (dedent! new-level)
    (displayln new-level)
    (displayln indent-stack)
    (let loop ([s indent-stack]
               [count 0])
      (cond
        [(< new-level (first s))
         (loop (rest s) (+ 1 count))]
        [(= new-level (first s))
         (set! indent-stack s)
         (make-list count '(DEDENT))]
        [(> new-level (first s))
         (error 'lexer "inconsistent indention")])))
  (define (handle-indent! spaces)
    (cond
      [(> spaces (current-indent))
       (indent! spaces)]
      [(= spaces (current-indent))
       '()]
      [(< spaces (current-indent))
       (dedent! spaces)]))
  (define (eof-dedent!)
    (append (handle-indent! 0) '((ENDMARKER))))

    
  ; TODO: actual unicode!
  (define-peg Lu (-char-range #\A #\Z))
  (define-peg Ll (-char-range #\a #\z))
  (define-peg Nd (-char-range #\0 #\9))
  
  (define-peg id-start (-or Lu Ll))
  (define-peg id-continue (-or id-start Nd))
  (define-peg identifier
    (=> (-capture s (-seq id-start (-* id-continue)))
        `(ID ,s)))

  (define-peg-macro -keywords
    (syntax-parser
      [(_ k ...)
       #:with (s ...) (stx-map (lambda (k) (datum->syntax #'here (symbol->string (syntax-e k))))
                               #'(k ...))
       #'(-or (-string s) ...)]))
  
  (define-peg keyword
    (=> (-capture
         s
         (-keywords
          False      await      else       import     pass
          None       break      except     in         raise
          True       class      finally    is         return
          and        continue   for        lambda     try
          as         def        from       nonlocal   while
          assert     del        global     not        with
          async      elif       if         or         yield))
        `(KEYWORD ,(string->symbol s))))

  (define-peg nonzerodigit (-char-range #\1 #\9))
  (define-peg digit (-char-range #\0 #\9))
  
  (define-peg integer (-or decinteger))
  (define-peg decinteger
    (-or (-seq nonzerodigit (-* (-seq (-? #\_) digit)))
         (-seq (-+ #\0) (-* (-seq (-? #\_) #\0))))) 
  
  (define-peg comment (-seq #\# (-many-until -any-char #\newline)))

  ; TODO: tabs
  (define-peg whitespace-char (-or #\space))
  (define-peg whitespace (-* whitespace-char))

  ; TODO: other line termination sequences
  (define-peg line-terminator (-or #\newline))

  (define-peg blank-line (-seq whitespace (-? comment)))

  ; TODO: a -case form could make the call to line-start a tail call.
  (define-peg line-start
    (-or
     (=> (-seq blank-line line-terminator rest:line-start)
         rest)
     (=> (-seq blank-line -eof)
         (eof-dedent!))
     (=> (-seq (-bind indent (=> (-capture s whitespace)
                                 ; TODO: Side-effect. Should I support effect backtracking?
                                 (handle-indent! (string-length s))))
               rest:line-continue)
         (append indent rest))))

  (define-peg line-continue
    (-or
     (=> -eof                                          ; EOF with dedents
         (cons '(NEWLINE) (eof-dedent!)))
     (=> (-seq (-? comment) line-terminator rest:line-start) ; newline, maybe after comment
         (cons '(NEWLINE) rest))
     (=> (-seq #\\ line-terminator rest:line-continue) ; line continuation
         rest)
     (=> (-seq whitespace-char rest:line-continue)     ; between-token whitespace
         rest)
     (=> (-seq k:keyword rest:line-continue)           ; keywords (before ids to override)
         (cons k rest))
     (=> (-seq id:identifier rest:line-continue)       ; identifier
         (cons id rest))
     ))

  (parse line-start str))

(lexer
 #<<here
# Comment 1
 foo
  bar baz
 baz False
 # foo
here
 )

; In general the (-seq pattern rest) and (cons token rest) patterns mean
; we're building up stack linear in program size.