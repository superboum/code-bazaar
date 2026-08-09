(letrec
  (find-divisor (lambda (n d)
    (if
      (eq (mod n d) 0)
      d
      (find-divisor n (+ d 1)))))
  (letrec
    (largest-prime-factor (lambda (n last-div)
      (if
        (< n last-div) 
        last-div
        (let 
	  (res (find-divisor n 2))
          (largest-prime-factor (/ n res) res)))))
    (largest-prime-factor 600851475143 0)))
