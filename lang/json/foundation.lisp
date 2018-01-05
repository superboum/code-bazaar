(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun sym-to-var (fn l s v)
  (funcall fn (lambda (y) (if (eq y s) v y)) l))

(defmacro pipeline ((as start) &body body)
  (reduce
    (lambda (acc x)
      (sym-to-var #'mapcar x as acc))
    body
    :initial-value start))

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
