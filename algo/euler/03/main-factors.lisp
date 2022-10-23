#!/usr/bin/sbcl --script

(defun find-divisor (n d)
  (cond
    ((eq (mod n d) 0) d)
    (t (find-divisor n (+ d 1)))
))

(defun largest-prime-factor (n last-div)
  (cond
    ((< n last-div) last-div)
    (t (let ((res (find-divisor n 2)))
         (largest-prime-factor (/ n res) res)
))))

(write (largest-prime-factor 600851475143 0))
