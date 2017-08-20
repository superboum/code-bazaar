#!/usr/bin/sbcl --script

(defun can-be-divided (n l)
  (cond
    ((eq l nil) nil)
    ((eq (mod n (car l)) 0) t)
    (t (can-be-divided n (cdr l)))
))

(defun prime (selected_number primes_list limit)
  (cond
    ((>= (list-length primes_list) limit) primes_list)
    ((can-be-divided selected_number primes_list) (prime (+ selected_number 1) primes_list limit))
    (t (prime (+ 1 selected_number) (cons selected_number primes_list) limit))
))

(write (car (prime 3 '(2) 10001)))
