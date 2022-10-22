(define (first-day-month month year) 
  (make-date 0 0 0 1 1 month year))

(define (first-weekday-month-fr month year)
  (mod (- (date-week-day (first-day-month month year)) 1) 7))

(define (num-days-in-month month year)
  (first-day-month month year))

(define (day->duration n) (make-time 'time-duration 0 (* 24 60 60 n)))
 
(define (date-add-days date days) 
  (time-utc->date (add-duration (date->time-utc date) (day->duration days))))

(define (days-feb year)
  (cond
    ((= (mod year 4) 0) 29)
    (#t 28)))

(define long-months (list 1 3 5 7 8 10 12))
(define (last-day-month month year)
  (cond
    ((= month 2) (days-feb year))
    ((member month long-months) 31)
    (#t 30)))


