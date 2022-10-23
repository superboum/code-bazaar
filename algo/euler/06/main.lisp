#!/usr/bin/sbcl --script

(defun square-sum (n)
  (cond
    ((<= n 0) 0)
    (t (+
        (square-sum (- n 1))
        (* n n)
))))

(defun sum (n)
  (cond
    ((<= n 0) 0)
    (t (+
        (sum (- n 1))
        n
))))

(defun sum-square (n)
  (* (sum n) (sum n))
)

(write (- (sum-square 100) (square-sum 100)))
