(defun decompose-number (x)
  (cond
    ((eq x 0) (list))
    (t (multiple-value-bind (q r) (floor x 10) (cons r (decompose-number q))))
))

(defun sum-given-power (x pow) (reduce #'+ (map 'list (lambda (x) (expt x pow)) (decompose-number x))))

(defun compute (counter limit pow)
  (cond
    ((> counter limit) (list))
    ((eq (sum-given-power counter pow) counter) (cons counter (compute (+ 1 counter) limit pow)))
    (t (compute (+ 1 counter) limit pow))
))

(print (reduce #'+ (compute 10 9999999 5))) ; 9999999 is arbitrary in our case.
; It would be a good idea to search a maximum value.
