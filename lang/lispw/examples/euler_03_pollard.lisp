(let* 
  ([g (lambda (n x) (mod (+ (* x x) 1) n))]
   [pollard (lambda (x y d n)
     (cond
       ((eq d n) nil)
       ((eq d 1)
         (let ((nx (g n x)) (ny (g n (g n y))))
           (pollard nx ny (gcd (abs (- nx ny)) n) n)
       ))
       (t d)))]
   [find-factors (lambda (n)
     (let ((factor (pollard 2 2 1 n)))
       (cond
         ((eq factor nil) (list n))
         (t (cons factor (find-factors (/ n factor)))))))])
  (reduce max (find-factors 600851475143)))
