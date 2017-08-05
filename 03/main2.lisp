#!/usr/bin/sbcl --script

(defun compute-primes (limit)
  (let ((primes '(2)))
    (loop for x from 3 to limit by 2 do
          (cond
            ((some (lambda (prime) (eq (mod x prime) 0)) primes) nil)
            (t (push x primes))
    ))
    primes
))

(defun largest-prime-factor (n)
  (some
    (lambda (x)
      (cond
        ((eq (mod n x) 0) x)
        (t nil)
      ))
     (compute-primes (isqrt n))
))

(print (largest-prime-factor 600851475143))
