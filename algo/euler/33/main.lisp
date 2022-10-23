; Similar to python range
(defun range (min max step)
  (cond
    ((>= min max) (list))
    (t (cons min (range (+ step min) max step)))
))

; Output every possible fraction lesser than one with a numerator and
; a denominator of 1 digit.
(defun one-digit-fractions ()
  (reduce
    #'append
    (map
      'list
      (lambda (y) (map
        'list
        (lambda (x) (list y x))
        (range (+ y 1) 10 1))
      ) (range 1 10 1)
)))

; From a one-digit-fraction, create a 2 digit-fraction with the same digit
; in the numerator and denominator
(defun transformation (f n)
  (let ((numerator (first f)) (denominator (second f)))
    (remove-if (lambda (x) (>= (first x) (second x)))
      (list
        (list (+ numerator (* 10 n)) (+ denominator (* 10 n)))
        (list (+ numerator (* 10 n)) (+ n (* 10 denominator)))
        (list (+ n (* 10 numerator)) (+ denominator (* 10 n)))
        (list (+ n (* 10 numerator)) (+ n (* 10 denominator)))
))))

(defun find-fractions (f)
(reduce
  (lambda (acc n)
    (append
      (remove-if
        (lambda (x) (not (=
          (/ (first f) (second f))
          (/ (first x) (second x))
          )))
          (transformation f n)
      )
      acc
  ))
  (range 1 10 1)
  :initial-value (list)
))
