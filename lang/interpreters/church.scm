;-- interpreter
(define (eval-expr expr env)
  ;(display (format "~a~n~n" expr))
  (cond
    ((symbol? expr) (env expr))
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
(define (empty-env x) (raise 'variable-unbound))
(define (lcalc-bool expr)
  (((eval-expr expr empty-env) #t) #f))

(define (lcalc-num expr)
  (((eval-expr expr empty-env) (lambda (v) (+ v 1))) 0))

; -- logic
(define stdenv 
   ; bool
  '(! faux (@ x (@ y y)) 
   (! vrai (@ x (@ y x))
   (! si (@ b (@ x (@ y ((b x) y) )))
   (! non (@ a (((si a) faux) vrai))
   (! et (@ a (@ b (((si a) b) faux)))
   (! ou (@ a (@ b (((si a) vrai) b)))
  
   ; int peano
   (! zero (@ f (@ x x))
   (! un (@ f (@ x (f x)))
   (! deux (@ f (@ x (f (f x))))

   deux
))))))))))
