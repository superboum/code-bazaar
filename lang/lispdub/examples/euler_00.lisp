(let (multiples-of-3-or-5 (lambda (n)
  (if (eq n 0) 
      0
      (if (or (eq (mod n 3) 0) (eq (mod n 5) 0))
          (+ n (multiples-of-3-or-5 (- n 1)))
          (multiples-of-3-or-5 (- n 1))))
)) (multiples-of-3-or-5 12))
