(letrec 
  (fizzbuzz (lambda (cnt acc)
    (cond
      [(eq cnt 0) acc]
      [(eq (mod cnt 15) 0) (fizzbuzz (- cnt 1) (cons "fizzbuzz" acc))]
      [(eq (mod cnt 3) 0)  (fizzbuzz (- cnt 1) (cons "fizz" acc))]
      [(eq (mod cnt 5) 0)  (fizzbuzz (- cnt 1) (cons "buzz" acc))]
      [t                   (fizzbuzz (- cnt 1) (cons cnt acc))])))
  (fizzbuzz 100 '()))
