#!/usr/bin/sbcl --script

(defun leap-year (x)
  (or
    (and (not (eq (mod x 100) 0)) (eq (mod x 4) 0)) ; not a century
    (and (eq (mod x 100) 0) (eq (mod x 400) 0)) ; is a century
))

(defun days-number-in-month (m y)
  (cond
    ((and (leap-year y) (eq m 2)) 29) ; Februrary leap year has 29 days
    ((eq m 2) 28) ; February not a leap year has 28 days
    ((find m '(4 6 9 11)) 30) ; april, june, september, november have 30 days
    (t 31) ; all the other months have 31 days
))

(defun next-day (d m y)
  (cond
    ((<= (+ 1 d) (days-number-in-month m y)) (list (+ 1 d) m y))
    ((< m 12) (list 1 (+ m 1) y))
    (t (list 1 1 (+ y 1)))
))

(defun compute (current-date stop-date current-week-day)
  (cond
    ((equal current-date stop-date) 0)
    ((and (eq current-week-day 7) (eq (first current-date) 1))
     ; (print current-date) -> debug
     (+ 1 (compute (apply #'next-day current-date) stop-date (+ (mod current-week-day 7) 1))))
    (t (compute (apply #'next-day current-date) stop-date (+ (mod current-week-day 7) 1)))
))

(write (compute (list 1 1 1901) (list 31 12 2000) 2))
