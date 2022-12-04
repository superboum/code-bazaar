(define (lex-int port)
  (string->number (list->string (lex-int-inner port))))

(define (lex-int-inner port)
  (let ([c (lookahead-char port)])
    (cond
      ((eof-object? c) '())
      ((char<=? #\0 c #\9) (cons (get-char port) (lex-int-inner port)))
      (#t '())
)))

(define (lex-token port t) (assert (eq? t (get-char port))))

(define (parse-interval port)
  (let* ([start (lex-int port) ]
        [_ (lex-token port #\-)]
        [end (lex-int port) ])
    `(,start . ,end)
))

(define (parse-section port)
  (let* ([left (parse-interval port)]
         [_1 (lex-token port #\,)]
         [right (parse-interval port)]
         [_2 (lex-token port #\newline)])
    `(,left . ( ,right ))
))

(define (inclusion? left right)
  (or (inclusion-left? left right) (inclusion-left? right left)))

(define (inclusion-left? left right)
  (and (>= (car left) (car right)) (<= (cdr left) (cdr right))))

(define (result port)
  (cond
    ((eof-object? (lookahead-char port)) 0)
    ((apply inclusion? (parse-section (current-input-port))) (+ 1 (result port)))
    (#t (result port))
))

(format #t "~a~%" (result (current-input-port)))

