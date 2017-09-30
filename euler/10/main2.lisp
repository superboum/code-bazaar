(defconstant limit 2000000)
(defconstant sievebound (floor (- limit 1) 2))
(defconstant crosslimit (floor (- (isqrt limit) 1) 2))

(defun solve-with-list ()
  (let (
      (sieve (make-list sievebound))
      (sum 2))

    ; Create sieve
    (loop
      for i from 1 to crosslimit
      when (not (nth (- i 1) sieve))
        do (loop for j from (* 2 i (+ i 1)) to sievebound by (+ (* 2 i) 1)
          do (setf (nth (- j 1) sieve) 1)
    ))

    ; Compute sum from sieve
    (loop
      for i from 1 to sievebound
      when (not (nth (- i 1) sieve)) do (setq sum (+ sum (* 2 i) 1))
    )

    sum
))

(defun solve-with-array ()
  (let (
      (sieve (make-array sievebound :element-type 'bit :initial-element 0))
      (sum 2))

    ; Create sieve
    (loop
      for i from 1 to crosslimit
      when (zerop (sbit sieve (- i 1)))
        do (loop for j from (* 2 i (+ i 1)) to sievebound by (+ (* 2 i) 1)
          do (setf (sbit sieve (- j 1)) 1)
    ))

    ; Compute sum from sieve
    (loop
      for i from 1 to sievebound
      when (zerop (sbit sieve (- i 1) )) do (setq sum (+ sum (* 2 i) 1))
    )

    sum
))

(print "Solve with a list")
(time (print (solve-with-list)))

(print "Solve with an array")
(time (print (solve-with-array)))
