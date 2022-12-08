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
  (let ([initial-state (parse-headers port)])
    (cond
      ((lex-token port #\newline) (extract-result (parse-moves port initial-state)))
      (#t 'error))
))

(define (parse-headers port)
  (let ([initial-state (parse-crates-top port '())])
    (parse-num-lines port)
    (prepare initial-state)
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
      ((lex-token port #\newline) (cons maybeCrate '()))
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
    letter
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

(define (parse-moves port state)
  (let ([c (lookahead-char port)])
    (cond
      ((eof-object? c) state)
      ((eq? c #\m) (parse-moves port (parse-move-line port state)))
      (#t 'error))))

(define (parse-move-line port state)
  (let* ([_1 (lex-word port "move ")]
         [count (lex-int port)]
         [_2 (lex-word port " from ")]
         [from (lex-int port)]
         [_3 (lex-word port " to ")]
         [to (lex-int port)])
  (lex-token port #\newline)
  (next-step state count from to)
))
  

;-- utils
(define (zip a b)
  (cond
    ((null? a) '())
    ((null? b) (cons (cons (car a) '()) (zip (cdr a) b)))
    (#t (cons (cons (car a) (car b)) (zip (cdr a) (cdr b))))))

(define (split src n sel)
  (cond
    ((null? src) (values sel src))
    ((<= n 0) (values sel src))
    (#t (split (cdr src) (- n 1) (cons (car src) sel)))
))

;-- logic
(define (prepare ll) (list->vector (map clear-empty (map reverse ll))))
(define (clear-empty l)
  (cond
    ((null? l) '())
    ((eq? 'empty (car l)) (clear-empty (cdr l)))
    (#t l)))

(define (next-step state count from-idx to-idx)
  (let ([from (vector-ref state (- from-idx 1))]
        [to (vector-ref state (- to-idx 1))])
    (let-values ([(sel rest) (split from count '())])
      (vector-set! state (- from-idx 1) rest)
      (vector-set! state (- to-idx 1) (append (reverse sel) to))
      state
)))

(define (extract-result state)
  (list->string (map (lambda (l) (car l)) (vector->list state))))

(format #t "~a~%" (parse-input (current-input-port)))
