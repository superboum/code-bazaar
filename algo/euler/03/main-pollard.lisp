#!/usr/bin/sbcl --script
; https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm

(defun g (n x) (mod (+ (* x x) 1) n))

(defun pollard (x y d n)
  (cond
    ((eq d n) nil)
    ((eq d 1)
      (let ((nx (g n x)) (ny (g n (g n y))))
        (pollard nx ny (gcd (abs (- nx ny)) n) n)
    ))
    (t d)
))

(defun find-factors (n)
  (let ((factor (pollard 2 2 1 n)))
    (cond
      ((eq factor nil) (list n))
      (t (cons factor (find-factors (/ n factor))))
)))

(write (reduce #'max (find-factors 600851475143)))
