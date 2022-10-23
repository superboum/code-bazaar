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

(defun all-pandigital-prods-r (a b max-a max-b)
  (cond
    ((> b max-b) (list))
    ((> a max-a) (print b) (all-pandigital-prods-r 1 (+ 1 b) max-a max-b))
    ((is-pandigital (list a b (* a b))) (cons (list a b (* a b)) (all-pandigital-prods-r (+ 1 a) b max-a max-b)))
    (t  (all-pandigital-prods-r (+ 1 a) b max-a max-b))
))

(defun all-pandigital-prods (max-a max-b) (all-pandigital-prods-r 1 1 max-a max-b))

(defun solution ()
  (reduce #'+
    (map
      'list
      (lambda (x) (first (last x)))
      (all-pandigital-prods 99999 99999)
)))

(print (solution))
