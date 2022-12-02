(define (collect)
  (let ([line (get-line (current-input-port))])
    (cond
      ((eof-object? line) '())
      (#t (cons line (collect))))))

(define (aggregate cal-max cal-cur)
  (let ([food (get-line (current-input-port))])
    (cond
      ((eof-object? food) (max cal-max cal-cur))
      ((eq? food "") (aggregate (max cal-max cal-cur) 0))
      (#t (aggregate cal-max (+ cal-cur (string->number food))))
)))

(format #t "~a" (aggregate 0 0))
