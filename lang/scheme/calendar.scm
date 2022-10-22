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
(define (last-day-month-date date)
  (make-date-only (last-day-month date) (date-month date) (date-year date)))

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

; -- base calendar
(define (cal-day-cell-base date)
  (list
    " ————"
    (format "| ~2,'0d " (date-day date))))

(define (cal-base fdm)
  (map 
    (lambda (w)(map (lambda (d) (cal-day-cell-base d)) w))
    (gen-month-per-week fdm)))


; -- closing boxes on the second to last
(define (close-box-pad x) (list " ————" ""))
(define (cal-last-full-week cal)
  (let* ([rcal (reverse cal)]
         [prev-week-fill (- 7 (length (car rcal)))])
    (reverse
      (cons (append (map close-box-pad (iota prev-week-fill)) (car rcal)) (cdr rcal)))))

; -- closing boxes on the right part
(define (fill-right week)
  (let ([rweek (reverse week)])
    (let-values ([(top mid) (apply values (car rweek))])
      (reverse
        (cons
          (list
            (format "~a ~%" top)
            (format "~a|~%" mid))
          (cdr rweek))))))
(define (cal-right-part cal) (map fill-right cal))

; -- handle shifting first week to the right col
(define cal-pad '("     " "     "))
(define (cal-shift-top cal)
  (let* ([shift (- 7 (length (car cal)))]
         [padding (map (lambda (cnt) cal-pad) (iota shift))])
    (cons (append padding (car cal)) (cdr cal))))

; -- handle closing bottom of the calendar
(define (cal-bottom fdm cal)
  (let* ([rcal (reverse cal)]
         [pad (+ (weekday-fr (last-day-month-date fdm)) 1)])
    (reverse (cons
      (map (lambda (_) (list " ————")) (iota pad))
      rcal))))

(define (cal-decorated date)
  (cal-bottom date
  (cal-shift-top
  (cal-right-part
  (cal-last-full-week
  (cal-base date))))))

;  (format ".——————————————————————————————————————.
;|          CALENDRIER ~a          |
;|——————————————————————————————————————|
;"
          

(define (cal-display cal) (printf (apply string-append (flatten (map flip cal)))))

(define (cal-quick month year) (cal-display (cal-decorated (first-day-month month year))))
