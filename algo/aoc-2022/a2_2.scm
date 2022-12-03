(define char-mapping '(
  (#\A . rock) (#\B . paper) (#\C . scissors)
  (#\X . lose) (#\Y . draw) (#\Z . win)
))

(define (parse . prepros)
  `(
    ,(cdr (assv (car prepros) char-mapping))
    ,(cdr (assv (caddr prepros) char-mapping))
))

(define win-next '(rock paper scissors rock))
(define loose-next (reverse win-next))
(define (get-next l e)
  (cond
    ((eq? (car l) e) (cadr l))
    (#t (get-next (cdr l) e))))

(define (scoring op exp-res me)
  (+
    (cond
      ((eq? me 'rock) 1)
      ((eq? me 'paper) 2)
      ((eq? me 'scissors) 3))
    (cond
      ((eq? exp-res 'lose) 0)
      ((eq? exp-res 'draw) 3)
      ((eq? exp-res 'win) 6))
))

(define (solve-action op exp-res)
  (cond
    ((eq? exp-res 'draw) (list op exp-res op))
    ((eq? exp-res 'win) (list op exp-res (get-next win-next op)))
    (#t (list op exp-res (get-next loose-next op)))
))

(define (pipe fn in)
  (cond 
    ((null? fn) in)
    (#t (pipe (cdr fn) (apply (car fn) (if (list? in) in (list in)))))
))

(define (result acc)
  (let ([line (get-line (current-input-port))])
    (cond
      ((eof-object? line) acc)
      (#t (result (+ acc (pipe (list string->list parse solve-action scoring) line))))
)))

(format #t "~a" (result 0))
