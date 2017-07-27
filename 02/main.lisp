#!/usr/bin/sbcl --script

(defun fibonacci (l max)
  (cond
    (
      (> (+ (car l) (car (cdr l))) max)
      l
    )
    (t (fibonacci (cons (+ (car l) (car (cdr l))) l) max))
  )
)

(defun even-sum (l)
  (cond
    ((eq l nil) 0)
    ((eq (mod (car l) 2) 1) (even-sum (cdr l)))
    (t (+ (car l) (even-sum (cdr l))))
  )
)

(write (even-sum (fibonacci '(2 1) 4000000)))
