#!/usr/bin/sbcl --script

(defun factorial (n)
  (cond
    ((eq n 0) 1)
    (t (* n (factorial (- n 1))))
))

(defun add_digits (n)
  (cond
    ((eq n 0) 0)
    (t (+ (mod n 10) (add_digits (floor n 10))))
))

(write (add_digits (factorial 100)))
