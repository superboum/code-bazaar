#!/usr/bin/sbcl --script

(defun can-be-divided (n l)
  (cond
    ((eq l nil) nil)
    ((eq (mod n (car l)) 0) t)
    (t (can-be-divided n (cdr l)))
))

(defun can-be-divided2 (n l)
  (some (lambda (x) (eq (mod n x) 0)) l)
)

(print "Recurse + prime list reversed each time")
(defun prime1 (selected_number primes_list limit)
  (cond
    ((>= (list-length primes_list) limit) primes_list)
    ((can-be-divided selected_number (reverse primes_list)) (prime1 (+ selected_number 1) primes_list limit))
    (t (prime1 (+ 1 selected_number) (cons selected_number primes_list) limit))
))
(time (first (prime1 3 '(2) 10001)))

(print "Recurse + prime list built reversed")
(defun prime2 (selected_number primes_list limit)
  (cond
    ((>= (list-length primes_list) limit) primes_list)
    ((can-be-divided selected_number primes_list) (prime2 (+ selected_number 1) primes_list limit))
    (t (prime2 (+ 1 selected_number) (append primes_list (list selected_number)) limit))
))
(time (first (last (prime2 3 '(2) 10001))))

(print "Use some + prime list reversed each time")
(defun prime3 (selected_number primes_list limit)
  (cond
    ((>= (list-length primes_list) limit) primes_list)
    ((can-be-divided2 selected_number (reverse primes_list)) (prime3 (+ selected_number 1) primes_list limit))
    (t (prime3 (+ 1 selected_number) (cons selected_number primes_list) limit))
))
(time (first (prime3 3 '(2) 10001)))

(print "Use some + prime list built reversed")
(defun prime4 (selected_number primes_list limit)
  (cond
    ((>= (list-length primes_list) limit) primes_list)
    ((can-be-divided2 selected_number primes_list) (prime4 (+ selected_number 1) primes_list limit))
    (t (prime4 (+ 1 selected_number) (append primes_list (list selected_number)) limit))
))
(time (first (last (prime4 3 '(2) 10001))))
