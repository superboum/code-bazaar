(let (a (lambda (x) (if (eq x 0) 0 (+ x (a (- x 1)))))) (a 3))
