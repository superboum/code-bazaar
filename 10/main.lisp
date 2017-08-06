#!/usr/bin/sbcl --script

(defun can-be-divided (n l stop)
  (cond
    ((> stop n) nil)
    ((eq l nil) nil)
    ((eq (mod n (car l)) 0) (car l))
    (t (can-be-divided n (cdr l) stop))
  )
)

(defun compute-primes (iterator limit primes)
  (cond
    ((>= iterator limit) primes)
    ((can-be-divided iterator primes (isqrt iterator)) (compute-primes (+ 2 iterator) limit primes))
    (t (compute-primes (+ 2 iterator) limit (append primes (list iterator))))
))

(print (reduce #'+ (compute-primes 3 2000000 '(2))))
