#!/usr/bin/sbcl --script

(defun solve ()
  (let ((in (read-line *standard-input* nil)))
    (cond
      ((eq in nil) 0)
      (t (+ (parse-integer in) (solve)))
)))

(write (parse-integer (subseq (write-to-string (solve)) 0 10)))

