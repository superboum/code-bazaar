(defpackage :life
  (:use :cl)
  (:export
    #:one-turn
    #:n-turn
    #:parse-life106
))

(in-package :life)

(defun comb (s1 s2 e1 e2)
  (labels ((rec (c1 c2)
    (cond
      ((>= c2 e2) nil)
      ((>= c1 e1) (rec s1 (+ 1 c2)))
      (t (cons (list c2 c1) (rec (+ 1 c1) c2)))
  )))
  (rec s1 s2)
))

(defun get-pos-around (cell)
  (map 'list (lambda (x) (mapcar #'+ cell x)) (comb -1 -1 2 2))
)

(defun count-neighbors (cell cell-list)
  (length (intersection (get-pos-around cell) cell-list :test 'equal))
)

(defun still-alive (candidates cell-list needed)
  (remove-if-not
    (lambda (cell)
      (some (lambda (x) (eq x (count-neighbors cell cell-list))) needed)
    )
    candidates
))

(defun get-potential-new (cell-list)
  (remove-if
    (lambda (y) (member y cell-list :test 'equal))
    (remove-duplicates
      (reduce (lambda (acc x) (append (get-pos-around x) acc)) cell-list :initial-value nil)
      :test 'equal
)))

(defun one-turn (cell-list)
  (append
    (still-alive cell-list cell-list '(3 4))
    (still-alive (get-potential-new cell-list) cell-list '(3))
))

(defun n-turn (cell-list counter)
  (cond
    ((<= counter 0) cell-list)
    (t (n-turn (one-turn cell-list) (- counter 1)))
))

(defun parse-life106 (stream)
  (labels (
  (read-int (l) (multiple-value-bind
    (n c)
    (parse-integer l :junk-allowed t)
    (cond
      ((not n) nil)
      (t (cons n (read-int (subseq l c))))
  )))
  (rec ()
    (let ((line (read-line stream nil)))
      (cond
        ((not line) nil)
        (t (cons (read-int line) (rec)))
  ))))
  (cond
    ((search "Life 1.06" (read-line stream)) (rec))
    (t nil)
)))

