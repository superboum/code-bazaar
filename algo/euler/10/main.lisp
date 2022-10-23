#!/usr/bin/sbcl --script

(defconstant limit 2000000)
(defconstant sievebound (floor (- limit 1) 2))
(defconstant crosslimit (floor (- (isqrt limit) 1) 2))

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

(write (solve-with-array))
