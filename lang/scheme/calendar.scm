(define (last l) (car (reverse l)))

(define (flip-first in pro rest)
  (cond
    ((null? in) (values (reverse pro) (reverse rest)))
    (#t (flip-first (cdr in) (cons (caar in) pro) (cons (cdar in) rest)))))

(define (flip l)
  (let-values ([(pro rest) (flip-first l '() '())])
    (cond
      ((null? (car rest)) (list pro))
      (#t (cons pro (flip rest))))))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))


(define (make-date-only day month year)
  (make-date 0 0 0 1 day month year))

(define (first-day-month month year) 
  (make-date-only 1 month year))

(define (day->duration n) (make-time 'time-duration 0 (* 24 60 60 n)))

(define (date-add-days date days)
  (time-utc->date (add-duration (date->time-utc date) (day->duration days))))

(define (weekday-fr date) (mod (- (date-week-day date) 1) 7))

(define (first-weekday-month-fr month year)
  (weekday-fr (first-day-month month year)))

(define (days-feb year)
  (cond
    ((= (mod year 4) 0) 29)
    (#t 28)))

(define long-months (list 1 3 5 7 8 10 12))
(define (last-day-month date)
  (cond
    ((= (date-month date) 2) (days-feb (date-year date)))
    ((member (date-month date) long-months) 31)
    (#t 30)))

(define (gen-week date)
  (cond
    ((= (weekday-fr date) 6) (list date))
    ((= (last-day-month date) (date-day date)) (list date))
    (#t (cons date (gen-week (date-add-days date 1))))))

(define (gen-month-per-week date)
  (let* ([week (gen-week date)]
           [lday (last week)])
    (cond
      ((= (last-day-month date) (date-day lday)) (list week))
      (#t (cons week (gen-month-per-week (date-add-days lday 1)))))))

(define cal-pad '("     " "     " "     "))
(define (cal-day-cell-base date)
  (list
    (format " ————")
    (format "| ~2,'0d " (date-day date))
    (format " ————")))

(define (cal-day-cell-end week)
  (let ([rweek (reverse week)])
    (let-values ([(top mid bot) (apply values (car rweek))])
      (reverse
        (cons
          (list
            (format "~a ~%" top)
            (format "~a|~%" mid)
            (format "~a ~%" bot))
          (cdr rweek))))))

(define (build-cells fdm)
  (map (lambda (w) (cal-day-cell-end (map (lambda (d) (cal-day-cell-base d)) w))) (gen-month-per-week fdm)))

(define (build-cells-with-pads fdm)
  (let ([days (build-cells fdm)]
        [pads (map (lambda (cnt) cal-pad) (iota (weekday-fr fdm)))])
    (cons (append pads (car days)) (cdr days))))

;(define (build-cal-inner fdm)
;  (build-cell-with-pads fdm)
    
