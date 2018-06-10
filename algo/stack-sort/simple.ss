(define (stack-sort stack-orig stack-temp)
  (cond
    ((null? stack-orig) (cdr stack-temp))
    ((and (not (null? stack-temp)) (< (car stack-orig) (car stack-temp)))
      (stack-sort
        (cons (car stack-orig) (cons (car stack-temp) (cdr stack-orig))) ; stack-orig
        (cdr stack-temp)))
    (#t (stack-sort (cdr stack-orig) (cons (car stack-orig) stack-temp)))))
