#!/usr/bin/sbcl --script

; 1 x 1000 = 1000 -> 9 digits
; 9 x 9999 = 89 991 -> 10 digits
; 10 x 100 = 1000 -> 9 digits
; 99 x 999 = 98901 -> 10 digits

(defun decompose-number (x)
  (cond
    ((eq x 0) (list))
    (t (multiple-value-bind (q r) (floor x 10) (cons r (decompose-number q))))
))

; x is a list like '(39 186 7254)
(defun is-pandigital (x)
  (let ((decompo (reduce #'append (map 'list #'decompose-number x))))
  (and (eq (length decompo) 9) (every (lambda (y) (find y decompo)) '(1 2 3 4 5 6 7 8 9)))
))

(defun compute (a b max-a max-b)
  (cond
    ((> b max-b) (list))
    ((> a max-a) (compute 1 (+ 1 b) max-a max-b))
    ((is-pandigital (list a b (* a b))) (cons (list a b (* a b)) (compute (+ 1 a) b max-a max-b)))
    (t (compute (+ 1 a) b max-a max-b))
))

(defun solution ()
  (reduce #'+
    (remove-duplicates
      (map
        'list
        (lambda (x) (first (last x)))
        (append (compute 10 100 99 999) (compute 1 9 1000 9999))
))))

(write (solution))
