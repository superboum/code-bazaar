#!/usr/bin/sbcl --script

(defun find-divisor (n d)
  (cond
    ((eq (mod n d) 0) d)
    (t (find-divisor n (+ d 1)))
))

(defun prime-factors (n last-div)
  (cond
    ((< n last-div) '())
    (t (let ((res (find-divisor n 2)))
         (cons res (prime-factors (/ n res) res))
))))

(defun divisor-counter (li la acc)
  (cond
    ((eq li '()) acc)
    ((eq la (first li)) (divisor-counter (rest li) la (+ 1 acc)))
    (t (* acc (divisor-counter (rest li) (first li) 2)))
))

(defun solve (n s lim)
  (cond
    ((> (divisor-counter (prime-factors s 2) 0 1) lim) s)
    (t (solve (+ n 1) (+ s n 1) lim))
))

(write (solve 1 1 500))
;(print (divisor-counter (prime-factors 28 2) 0 1))
