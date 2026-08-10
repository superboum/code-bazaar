(let* 
  ([g (lambda (n x) (mod (+ (* x x) 1) n))]
   [pollard (letrec 
     [do (lambda (x y d n)
       (cond
         ((eq d n) nil)
         ((eq d 1)
           (let* [(nx (g n x)) (ny (g n (g n y)))]
             (do nx ny (gcd (abs (- nx ny)) n) n)))
         (t d)))] 
     do)]
   [find-factors (letrec 
     [do (lambda (n)
       (let (factor (pollard 2 2 1 n))
         (cond
           ((eq factor nil) (list n))
           (t (cons factor (do (/ n factor)))))))]
      do)])
  (reduce max 0 (find-factors 600851475143)))
