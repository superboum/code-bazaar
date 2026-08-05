(let (fib (lambda (pprev prev counter)
  (if 
    (eq counter 1) 
    prev
    (fib prev (+ prev pprev) (- counter 1)))))
  (fib 0 1 30))
