(defun first-permutation (head tail)
  (cond
    ((eq nil head) (list))
    (t (cons
      (list (first head) (append (rest head) tail))
      (first-permutation (rest head) (cons (first head) tail))
))))

(defun extract (x)
  (map 'list (lambda (y) (append (butlast x) y)) (first-permutation (first (last x)) (list)))
)

(defun repermute (x)
  (cond
    ((eq x nil) '())
    (t
      (append (extract (first x)) (repermute (rest x)))
  )))

(defun repermute-loop (l)
  (cond
    ((not (listp (first l))) (repermute-loop (first-permutation l (list))))
    ((eq (first (last (first l))) nil) (map 'list #'butlast l))
    (t (repermute-loop (repermute l)))
))

(print (repermute-loop '(1 2 3 4 5 6 7 8 9)))
