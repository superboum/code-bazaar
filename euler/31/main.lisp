(defconstant coins (list 1 2 5 10 20 50 100 200 ))

(defun gen-possibilities (current target avaible-coins)
  (cond
    ((eq avaible-coins nil) (list))
    (t (cons
      (list (+ current (first avaible-coins)) target avaible-coins)
      (gen-possibilities current target (rest avaible-coins))
))))

(defun how-many-ways (current target avaible-coins)
  (cond
    ((eq current target) 1)
    ((> current target) 0)
    (t (reduce #'+ (map
      'list
      (lambda (x) (apply #'how-many-ways x))
      (gen-possibilities current target avaible-coins)
)))))

(print (how-many-ways 0 200 coins))
