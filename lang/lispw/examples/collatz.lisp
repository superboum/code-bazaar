(letrec (collatz (lambda (val counter)
  (if (eq val 1) 
    counter
    (if (eq (mod val 2) 0) 
      (collatz (/ val 2) (+ counter 1))
      (collatz (+ (* 3 val) 1) (+ counter 1))))))
  (collatz 27 0)
)
