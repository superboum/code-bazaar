(define (lex-int port)
  (string->number (list->string (lex-int-inner port))))

(define (lex-int-inner port)
  (let ([c (lookahead-char port)])
    (cond
      ((eof-object? c) '())
      ((char<=? #\0 c #\9) (cons (get-char port) (lex-int-inner port)))
      (#t '())
)))

(define (lex-token port t) 
  (let ([c (lookahead-char port)])
    (cond 
      ((eq? t c) (get-char port))
      (#t #f)
)))

; input : headers, moves
; moves : moveLine, moves | empty
; headers : crates, numLine
; crates: crateLine, crates | empty
; moveLine : 'move', int, 'from', int, 'to', int, newline 
; numLine : numBlock, numLine | '\n'
; crateLine : maybeCrate, ' ', crateLine | '\n'
; maybeCrate : (crate | emptySpace)
; crate : '[' int ']'
; empty : '<space><space><space>'
; numBlock : '<space>' int '<space>'

(define (parse-input port)
  (list
    'input
    (parse-headers port)
    ;(parse-moves port)
))

(define (parse-headers port)
  (list 
    'headers
    (cons 'crates (parse-crates port '()))
    ;(cons 'idx (parse-num-lines port)
))

(define (parse-crates port acc)
  (let ([line (parse-crate-line port)])
    (cond 
      ((null? line) acc)
      (#t (parse-crates port (zip line acc)))
)))

(define (parse-crate-line port)
  (let ([maybeCrate (parse-maybe-crate port)])
    (cond
      ((lex-token port #\newline) (list maybeCrate))
      ((lex-token port #\space) (cons maybeCrate (parse-crate-line port)))
      (#t '())
)))

(define (parse-maybe-crate port)
  (let ([c (lookahead-char port)])
    (cond
      ((eq? c #\[) (parse-crate port))
      ((eq? c #\space) (parse-empty-space port))
      (#t '())
)))

(define (parse-crate port)
  (lex-token port #\[)
  (let ([letter (get-char port)])
    (lex-token port #\])
    `(crate . ,letter)
))

(define (parse-empty-space port)
  (lex-token port #\space)
  (lex-token port #\space)
  (lex-token port #\space)
  'empty
)

;-- utils
(define (zip a b)
  (cond
    ((null? a) '())
    ((null? b) (cons (cons (car a) '()) (zip (cdr a) b)))
    (#t (cons (cons (car a) (car b)) (zip (cdr a) (cdr b))))))

(format #t "~a~%" (parse-input (current-input-port)))
