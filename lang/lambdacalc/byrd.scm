;-- interpreter
(define (eval-expr expr env)
  (cond
    ((symbol? expr) (env expr))
    ((eq? (car expr) 'f) 
      (let ([arg (cadr expr)]
            [body (caddr expr)])
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
  ;(((eval-expr expr empty-env) #t) #f))
  (eval-expr expr empty-env))

(define (lcacl-num expr)
  ((eval-expr expr empty-env) (lambda (v) (+ v 1)) 0))
