#!/usr/bin/sbcl --script

(defun primes (limit)
  (let (
      (sievebound (floor (- limit 1) 2))
      (crosslimit (floor (- (isqrt limit) 1) 2))
      (sieve (make-array (floor (- limit 1) 2) :element-type 'bit :initial-element 0)))

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
      when (zerop (sbit sieve (- i 1) )) collect (+ (* 2 i) 1)
    )
))

(defconstant max-prime 15000)
(defconstant lprimes (primes max-prime))

; Curryfication
(defun suite (a b)
  (lambda (n) (+ (* n n) (* a n) b))
)

(defun count-primes (f n)
  (let ((res (funcall f n)))
    (cond
      ((> res max-prime) (error "~S is too big for our prime list." res))
      ; @TODO define a find function that take in account the fact that the array is sorted.
      ((find res lprimes) (count-primes f (+ 1 n))) ; tant que c'est un premier
      (t n) ; condition d'arret
)))

(defun set-max (a b maxprimes maxa maxb)
  (let ((current-primes (count-primes (suite a b) 0)))
    (cond
      ((> current-primes maxprimes) (check-limits (+ 1 a) b current-primes a b))
      (t (check-limits (+ 1 a) b maxprimes maxa maxb))
)))

(defun check-limits (a b maxprimes maxa maxb)
  (cond
    ((> b 1000) (list maxprimes maxa maxb))
    ((>= a 1000)
      ;(print (list a b maxprimes maxa maxb))
      (check-limits -999 (+ b 1) maxprimes maxa maxb))
    (t (set-max a b maxprimes maxa maxb))
))

(defun test ()
  ; @TODO Better filtering could improve performance
  (let ((res (check-limits -999 0 -1 0 0))) ; If b < 0, the formula doesn't produce any prime
    (* (second res) (third res))
))

(write (test))
;(print (count-primes (suite -79 1601) 0)) ; should be equal to 80
