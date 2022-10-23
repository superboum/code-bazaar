#!/usr/bin/sbcl --script

(defun can-be-divided (n l stop)
  (cond
    ((> (first l) stop) nil)
    ((eq l nil) nil)
    ((eq (mod n (car l)) 0) t)
    (t (can-be-divided n (cdr l) stop))
))

(defun prime (selected_number primes_list limit)
  (cond
    ((>= (list-length primes_list) limit) primes_list)
    ((can-be-divided selected_number primes_list (isqrt selected_number)) (prime (+ selected_number 2) primes_list limit))
    (t (prime (+ 2 selected_number) (append primes_list (list selected_number)) limit))
))

(write (first (last (prime 3 '(2) 10001))))
