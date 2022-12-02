(define (take l n)
  (cond
    ((eq? n 0) '())
    ((null? l) '())
    (#t (cons (car l) (take (cdr l) (- n 1))))))

(define (nbig l n) (take (sort > l) n))

(define (aggregate cal-maxes cal-cur)
  (let ([food (get-line (current-input-port))])
    (cond
      ((eof-object? food) (apply + (nbig (cons cal-cur cal-maxes) 3)))
      ((eq? food "") (aggregate (nbig (cons cal-cur cal-maxes) 3) 0))
      (#t (aggregate cal-maxes (+ cal-cur (string->number food))))
)))

(format #t "~a" (aggregate '(0 0 0) 0))
