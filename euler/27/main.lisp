#!/usr/bin/sbcl --script

(defun can-be-divided (n l)
  (cond
    ((eq l nil) nil)
    ((eq (mod n (car l)) 0) t)
    (t (can-be-divided n (cdr l)))
))

(defun prime (selected_number primes_list limit)
  (cond
    ((>= (list-length primes_list) limit) primes_list)
    ((can-be-divided selected_number primes_list) (prime (+ selected_number 1) primes_list limit))
    (t (prime (+ 1 selected_number) (cons selected_number primes_list) limit))
))

(defconstant computed-primes 20000)
(defconstant lprimes (prime 3 '(2) computed-primes))

; Curryfication
(defun suite (a b)
  (lambda (n) (+ (* n n) (* a n) b))
)

(defun count-primes (f n)
  (let ((res (funcall f n)))
    (cond
      ((> res computed-primes) (error "~S is too big for our prime list." res))
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
  (let ((res (check-limits -999 0 -1 0 0))) ; If b < 0, the formula doesn't produce any prime
    (* (second res) (third res))
))

(write (test))
;(print (count-primes (suite -79 1601) 0)) ; should be equal to 80
