(define (bigger stack-orig stack-temp)
  (cond
    ((null? stack-orig) (values #t '() '() '()))
    ((or (null? stack-temp) (>= (car stack-orig) (car stack-temp)))
      (bigger
        (cdr stack-orig)
        (cons (car stack-orig) stack-temp)))
    (#t (values #f (cdr stack-orig) stack-temp (car stack-orig)))))

(define (restack stack-orig stack-temp pivot)
  (cond
    ((and (null? stack-temp) pivot) (cons pivot stack-orig))
    ((null? stack-temp) stack-orig)
    ((and pivot (>= pivot (car stack-temp))) (restack (cons pivot stack-orig) stack-temp #f))
    (#t (restack (cons (car stack-temp) stack-orig) (cdr stack-temp) pivot))))

(define (stack-sort stack)
  (let-values (((done stack-orig stack-temp pivot) (bigger stack '())))
    (cond
      (done stack)
      (#t (stack-sort (restack stack-orig stack-temp pivot))))))

(write (stack-sort (read)))
(newline)
(exit)
