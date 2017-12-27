(defun filter (start &rest body)
  (reduce
    (lambda (acc x) (funcall x acc))
    body
    :initial-value start))

(defun mapcar2 (fn ll) (mapcar (lambda (l) (mapcar fn l)) ll))

(defmacro filter2 ((as start) &body body)
  `(reduce
     (lambda (acc x)
       (apply
         (symbol-function (first x))
         (mapcar (lambda (y) (if (eq y ',as) acc y)) (rest x) )))
     '(,@body)
     :initial-value ,start))

(defmacro _curry (fn rev &rest start-args)
  (let ((fncall (symbol-function fn)))
    `(lambda (&rest end-args)
       (apply
         ,fncall
         (cond
           (,rev (append end-args (list ,@start-args)))
           (t (append (list ,@start-args) end-args))
)))))

(defmacro curry (fn &rest start-args) `(_curry ,fn nil ,@start-args))
(defmacro rcurry (fn &rest start-args) `(_curry ,fn t ,@start-args))
