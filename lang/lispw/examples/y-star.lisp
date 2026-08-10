(let*
  ([even-gen (lambda (even odd)
     (lambda (n) (if (eq n 0) t (odd (- n 1)))))]

   [odd-gen (lambda (even odd)
     (lambda (n) (if (eq n 0) nil (even (- n 1)))))]

   [even-odd (Y* even-gen odd-gen)]
  
   [even? (car even-odd)]
   [odd? (cadr even-odd)]
   [test1 (if (even? 12) 1 0)]
   [test2 (if (even? 13) 0 2)]
   [test3 (if (odd? 12) 0 4)]
   [test4 (if (odd? 13) 8 0)])
  (reduce + 0 (list test1 test2 test3 test4)))
  
