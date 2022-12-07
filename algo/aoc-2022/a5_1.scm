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

(define (lex-word port w)
  (let ([r (get-string-n port (string-length w))])
    (assert (string=? r w))))

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
    (cond
      ((lex-token port #\newline) (parse-moves port))
      (#t 'error))
))

(define (parse-headers port)
  (list 
    'headers
    (cons 'crates (parse-crates-top port '()))
    (cons 'idx (parse-num-lines port))
))

(define (parse-crates-top port acc)
  (let ([c (lookahead-char port)])
    (cond 
      ((eq? c #\space) (parse-crates-top port (zip (parse-crate-line port) acc)))
      ((eq? c #\[) (parse-crates-bottom port acc))
      (#t (cons 'error acc))
)))

(define (parse-crates-bottom port acc)
  (let ([c (lookahead-char port)])
    (cond 
      ((eq? c #\[) (parse-crates-bottom port (zip (parse-crate-line port) acc)))
      ((eq? c #\space) acc)
      (#t (cons 'error acc))
)))

(define (parse-crate-line port)
  (let ([maybeCrate (parse-maybe-crate port)])
    (cond
      ((lex-token port #\newline) (list maybeCrate))
      ((lex-token port #\space) (cons maybeCrate (parse-crate-line port)))
      (#t '(error))
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

(define (parse-num-lines port)
  (let ([num-block (parse-num-block port)])
    (cond
      ((lex-token port #\newline) (list num-block))
      ((lex-token port #\space) (cons num-block (parse-num-lines port)))
      (#t '(error))
)))

(define (parse-num-block port)
  (lex-token port #\space)
  (let ([v (lex-int port)])
    (lex-token port #\space)
    v))

(define (parse-moves port)
  (let ([c (lookahead-char port)])
    (cond
      ((eof-object? c) '())
      ((eq? c #\m) (cons (parse-move-line port) (parse-moves port)))
      (#t 'error))))

(define (parse-move-line port)
  (let* ([_1 (lex-word port "move ")]
         [count (lex-int port)]
         [_2 (lex-word port " from ")]
         [from (lex-int port)]
         [_3 (lex-word port " to ")]
         [to (lex-int port)])
  (lex-token port #\newline)
  `((count . ,count) (from . ,from) (to . ,to))
))
  

;-- utils
(define (zip a b)
  (cond
    ((null? a) '())
    ((null? b) (cons (cons (car a) '()) (zip (cdr a) b)))
    (#t (cons (cons (car a) (car b)) (zip (cdr a) (cdr b))))))

(format #t "~a~%" (parse-input (current-input-port)))
