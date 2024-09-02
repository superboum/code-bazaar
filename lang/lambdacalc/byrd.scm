(define (eval-expr expr env)
  (cond
    ((number? expr) expr)
    ((symbol? expr) (env expr))
    ((eq? (car expr) 'lambda) 
      (let ([arg (cadr expr)]
            [body (cadr expr)])
      (lambda (y) (eval-expr body 
        (lambda (r) (if (eq? r arg) y (env r)))))))
    (#t
      ((eval-expr (car expr) env) ; operator
       (eval-expr (cadr expr) env) ; operand
    ))
))
