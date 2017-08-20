#!/usr/bin/sbcl --script

(defun extract (l s)
  (cond
    ((eq s 0) '())
    ((eq l '()) nil)
    (t (cons (first l) (extract (rest l) (- s 1))))
))

(defun compute (adjacent greatest l)
  (let ((factors (extract l adjacent)))
    (cond
      ((eq (last factors) nil) greatest)
      ((> (reduce #'* factors) greatest) (compute adjacent (reduce #'* factors) (rest l)))
      (t (compute adjacent greatest (rest l)))
)))

(defun parse ()
  (let ((line (read-line *standard-input* nil)))
    (cond
      ((eq line nil) '())
      (t (append (map 'list #'digit-char-p (coerce line 'list)) (parse)))
)))

(write (compute 13 0 (parse)))
