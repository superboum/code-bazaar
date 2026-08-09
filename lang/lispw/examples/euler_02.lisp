(letrec (fibonacci (lambda (l maxv)
  (if (> (+ (car l) (car (cdr l))) maxv)
    l
    (fibonacci (cons (+ (car l) (car (cdr l))) l) maxv))))
  (let (even-sum (lambda (l)
    (if (eq l nil) 
      0
      (if (eq (mod (car l) 2) 1) 
	(even-sum (cdr l))
        (+ (car l) (even-sum (cdr l)))))))
    (even-sum (fibonacci (quote (2 1)) 4000000))))
