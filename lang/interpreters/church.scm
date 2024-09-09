;-- interpreter
(define (eval-expr expr env)
  ;(display (format "~a~n~n" expr))
  (cond
    ((symbol? expr) (env expr))
    ((not (list? expr)) (raise (format "`~a` is not a valid syntax" expr)))
    ((eq? (car expr) '!)
      (let ([k (cadr expr)] [v (eval-expr (caddr expr) env)] [rest (cadddr expr)])
	(eval-expr rest (lambda (r) (if (eq? r k) v (env r))))))
    ((eq? (car expr) '@) 
      (let ([arg (cadr expr)] [body (caddr expr)])
      (lambda (y) (eval-expr body 
        (lambda (r) (if (eq? r arg) y (env r)))))))
    (#t 
      ((eval-expr (car expr) env) ; operator
       (eval-expr (cadr expr) env) ; operand
    ))
))

;-- wrappers
(define (empty-env x) (raise (format "`~a` is not bound" x)))
(define (lcalc-bool expr)
  (((eval-expr expr empty-env) #t) #f))

(define (lcalc-num expr)
  (((eval-expr expr empty-env) (lambda (v) (+ v 1))) 0))

; -- logic
(define (with-stdenv body)
   ; bool
  `(! faux (@ x (@ y y)) 
   (! vrai (@ x (@ y x))
   (! si (@ b (@ x (@ y ((b x) y) )))
   (! non (@ a (((si a) faux) vrai))
   (! et (@ a (@ b (((si a) b) faux)))
   (! ou (@ a (@ b (((si a) vrai) b)))
  
   ; peano ops
   (! zero? (@ n ((n (@ x faux)) vrai))
   (! suiv (@ n (@ f (@ x (f ((n f) x)) )))
   (! plus (@ m (@ n (@ f (@ x ((m f) ((n f) x)) ))))
   (! mult (@ m (@ n (@ f (@ x ((m (n f)) x)))))

   (! pred (@ n (@ f (@ x 
     (((n 
	 (@ g (@ h (h (g f))))) ; inc
         (@ u x)) ; const
         (@ u u)) ; extract *k*
   )))
 
   ; peano shortcut
   (! n0 (@ f (@ x x))
   (! n1 (@ f (@ x (((suiv n0) f) x)))
   (! n2 (@ f (@ x (((suiv n1) f) x)))
   (! n3 (@ f (@ x (((suiv n2) f) x)))
   (! n4 (@ f (@ x (((suiv n3) f) x)))
   (! n5 (@ f (@ x (((suiv n4) f) x)))
   (! n6 (@ f (@ x (((suiv n5) f) x)))

   ; y combinator / fixed-point combinator
   ; https://8dcc.github.io/programming/understanding-y-combinator.html
   (! y (@ f 
     ((@ x (f (@ n ((x x) n))))
      (@ x (f (@ n ((x x) n))))))

   ; math
   (! presque-fact (@ r (@ n
     ((((si (zero? n))
            (@ n n1)) 
            ; force lazy exec.
            (@ n ((mult n) (r (pred n)))))
      n)
   ))
   (! fact (y presque-fact)

   ,body
))))))))))))))))))))))
