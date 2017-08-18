#!/usr/bin/sbcl --script

(defun extract-word-r (s l)
  (cond
    ((string-equal s "") "")
    ((eq (char s 0) #\Space) "")
    (t (concatenate 'string (subseq s 0 1) (extract-word-r (subseq s 1 l) (- l 1))))
))

(defun extract-word (s)
  (let ((init-length (length s)))
    (let ((word (extract-word-r s init-length)))
      (list word (string-trim " " (subseq s (length word) init-length)))
)))

(defun split-space (s)
  (cond
    ((string-equal s "") ())
    (t
      (let ((res (extract-word s)))
        (cons (car res) (split-space (second res)))
))))

(defun parse ()
  (let ((line (read-line *standard-input* nil)))
    (cond
      ((eq line nil) '())
      (t (cons (map 'list #'parse-integer (split-space line)) (parse)))
)))

(defun max-prod-adj-line-r (line l adjs)
  (cond
    ((> adjs l) 0)
    (t (max (reduce #'* (subseq line 0 adjs)) (max-prod-adj-line-r (subseq line adjs l) (- l adjs) adjs)))
))

(defun max-prod-adj-line (lines adjs)
  (let ((l (length (car lines))))
    (reduce #'max (map 'list (lambda (x) (max-prod-adj-line-r x l adjs)) lines))
))

(defun max-prod-col (lines)
  (cond
    ((eq (first lines) '()) 0)
    (t
      (max (reduce #'* (map 'list #'first lines)) (max-prod-col (map 'list #'rest lines))))
))

(defun max-prod-adj-col-r (lines l adjs)
  (cond
    ((< l adjs) 0)
    (t
      (max (max-prod-col (subseq lines 0 adjs)) (max-prod-adj-col-r (rest lines) (- l 1) adjs))
)))

(defun max-prod-adj-col (lines adjs) (max-prod-adj-col-r lines (length lines) adjs))

(defun pad-left-to-right (lines left right)
  (cond
    ((< left 0) '())
    (t (cons
         (append (make-sequence 'list left :initial-element 0) (first lines) (make-sequence 'list right :initial-element 0))
         (pad-left-to-right (rest lines) (- left 1) (+ right 1))
))))

(defun max-prod-adj-diag-r (lines l adjs)
  (cond
    ((< l adjs) 0)
    (t
      (max
        (max-prod-col (pad-left-to-right (subseq lines 0 adjs) (- adjs 1) 0))
        (max-prod-col (pad-left-to-right (reverse (subseq lines 0 adjs)) (- adjs 1) 0))
        (max-prod-adj-diag-r (rest lines) (- l 1) adjs)
))))

(defun max-prod-adj-diag (lines adjs) (max-prod-adj-diag-r lines (length lines) adjs))

(defun max-prod-adj (lines adjs)
  (reduce #'max (map 'list (lambda (x) (funcall x lines adjs)) (list #'max-prod-adj-line #'max-prod-adj-col #'max-prod-adj-diag)))
)

(print (max-prod-adj (parse) 4))
;(print (max-prod-col '( (1 1 1 1) (1 2 3 4) (1 1 1 1))))
;(print (pad-left-to-right (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12) (list 13 14 15 16)) 3 0))
