(letrec 
  (counter (lambda (init) (cons init (counter (+ 1 init)))))
  (cadddr (counter 0))) ; expect 3
