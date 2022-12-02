(define char-mapping '(
  (#\A . rock) (#\B . paper) (#\C . scissors)
  (#\X . rock) (#\Y . paper) (#\Z . scissors)
))

(define (line->game line)
  (let ([prepros (string->list line)])
    `(
      ,(cdr (assv (car prepros) char-mapping))
      ,(cdr (assv (caddr prepros) char-mapping))
    )))

(define (win? winner looser)
  (or 
    (and (eq? looser 'rock) (eq? winner 'paper))
    (and (eq? looser 'paper) (eq? winner 'scissors))
    (and (eq? looser 'scissors) (eq? winner 'rock))))

(define (match-score op me)
  (cond
    ((win? me op) 6)
    ((win? op me) 0)
    (#t 3)))

(define (match-play me)
  (cond
    ((eq? me 'rock) 1)
    ((eq? me 'paper) 2)
    ((eq? me 'scissors) 3)))

(define (match op me) (+ (match-score op me) (match-play me)))

(define (result acc)
  (let ([line (get-line (current-input-port)) ])
    (cond
      ((eof-object? line) acc)
      (#t (result (+ acc (apply match (line->game line)))))
)))

(format #t "~a" (result 0))
