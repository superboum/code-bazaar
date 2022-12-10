; -- utils
(define (list-contains-string? l s)
  (cond
    ((null? l) #f)
    ((string=? (car l) s) #t)
    (#t (list-contains-string? (cdr l) s))
))

; --- lexer
; syntax: $
; keywords: cd, ls, dir
; types: word, int

(define (lex-word port)
  (let ([c (get-char port)])
    (cond
      ((or (eof-object? c) (eq? c #\space) (eq? c #\newline)) '())
      (#t (cons c (lex-word port)))
)))

(define kw '("cd" "ls" "dir"))
(define (lexer port)
  (let* ([cw (lex-word port)] [w (list->string cw)])
    (cond
      ((null? cw) 'eof)
      ((string=? w "$") 'prompt)
      ((list-contains-string? kw w) (string->symbol w))
      ((and (char>=? (car cw) #\0) (char<=? (car cw) #\9)) `(int . ,(string->number w)))
      (#t  `(word . ,w))
)))
(define (lexer-peekable port)
  (let ([tmp (lexer port)])
    (lambda (opt)
      (cond
        ((eq? opt 'peek) tmp)
        (#t (let ([res tmp]) (set! tmp (lexer port)) res))
))))

; --- parser
; input: terminalLine, input | <eof>
; terminalLine: cmdLine | infoLine
; cmdLine: <$>, (<cd>, path | <ls>)
; path: <word>
; infoLine: dirLine | fileLine
; dirLine: <dir> <word>
; fileLine: <int> <word>

(define (parser tokens)
  (let ([t (tokens 'peek)])
    (cond
      ((eq? 'eof t) '())
      (#t (cons (parse-term tokens) (parser tokens)))
)))

(define (parse-term tokens)
  (let ([t (tokens 'peek)])
    (cond
      ((eq? 'prompt t) (parse-cmd tokens))
      ((or (eq? 'dir t) (eq? 'int (car t))) (parse-info tokens))
      (#t (raise "invalide token for parse-term"))
)))

(define (parse-cmd tokens)
  (assert (eq? (tokens 'read) 'prompt))
  (let ([t (tokens 'read)])
    (cond
      ((eq? t 'cd) `(cd ,(cdr (tokens 'read)))) ; we should type-check the path...
      ((eq? t 'ls) `(ls))
      (#t (raise "invalid token for parse-cmd"))
)))

(define (parse-info tokens)
  (let ([t (tokens 'read)])
    (cond 
      ((eq? 'dir t) `(dir ,(cdr (tokens 'read))))
      ((eq? 'int (car t)) `(file ,(cdr t) ,(cdr (tokens 'read))))
      (#t (raise "invalid token for parse-info"))
)))

  

; --- glue
(define lex-stdin (lexer-peekable (current-input-port)))
(format #t "~a~%" (parser lex-stdin))
