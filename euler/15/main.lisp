#!/usr/bin/sbcl --script

; Based on the Delannoy array but with a Pascal Triangle instead of a tribonacci triangle
; https://en.wikipedia.org/wiki/Delannoy_number

; Of a Pascal Triangle
(defun next-line (l last)
  (cond
    ((eq l nil) (list last))
    (t (cons (+ last (first l)) (next-line (rest l) (first l))))
))

(defun get-nth-line (n)
  (cond
    ((eq n 0) '(1))
    (t (next-line (get-nth-line (- n 1)) 0))
))

(write (reduce #'max (get-nth-line (* 20 2))))
