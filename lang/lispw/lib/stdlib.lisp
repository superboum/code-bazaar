; ---
; Constants
; ---
(define t (quote t))

; ---
; Recursivity
; ---
(define Y (lambda (f) 
  ((lambda (x) (f (x x))) 
   (lambda (x) (f (x x))))))

; Derived from https://okmij.org/ftp/Computation/fixed-point-combinators.html#Poly-variadic
;(define Y*
;  (lambda fns  ; 'fns' captures variadic arguments as a list
;    ((lambda (u) (u u))
;     (lambda (p)
;       (map (lambda (li) (apply li (p p))) fns)))))

; ---
; Boolean logic
; ---
(define and (lambda (x y) (if x y nil)))
(define or (lambda (x y) (if x t y)))

; ---
; Numbers
; ---
(define max (lambda (a b) (if (> a b) a b)))
(define min (lambda (a b) (if (< a b) a b)))
(define abs (lambda (a) (if (>= a 0) a (* a -1))))
(define gcd (lambda (a b) (if (eq b 0) a (gcd b (mod a b)))))

; ---
; List manipulation
; ---
(define null? (lambda (maybe_lst) (eq maybe_lst nil)))
(define list (lambda args args))
(define map (lambda (fn lst)
  (if lst (cons (fn (car lst)) (map fn (cdr lst))) nil)))
(define reduce (lambda (fn acc lst)
  (if lst (reduce fn (fn acc (car lst)) (cdr lst)) acc)))

; ---
; AST manipulation (useful for macros)
; ---
; ~build primitives more easily
(define ast-lambda (lambda (args body) 
  (cons (quote lambda) (cons args (cons body nil)))))

(define ast-let (lambda (bind body)
  (cons 
    (quote let) 
    (cons 
      (cons (car bind) (cons (cadr bind) nil)) 
      (cons body nil)))))

(define ast-if (lambda (predicate consequent alternative)
  (cons (quote if) (cons predicate (cons consequent (cons alternative nil))))))

; ~call stdlib functions more easily
(define ast-y (lambda (name body) 
  (cons (quote Y) (cons (ast-lambda (cons name nil) body) nil))))

; ~extend the syntax
; ~~let things
(define ast-letrec (lambda (bind body) 
  (ast-let 
    (cons (car bind) (cons (ast-y (car bind) (cadr bind)) nil))
    body)))

(define ast-let* (lambda (many_binds body)
  (if 
    (null? many_binds) 
    body
    (ast-let 
      (car many_binds) 
      (ast-let* (cdr many_binds) body)))))

; ~~cond things
(define ast-cond (lambda options
  (letrec 
    [do (lambda (options)
      (if 
        (null? options)
        nil
        (ast-if 
          (car (car options)) 
          (cadr (car options))
          (do (cdr options)))))]
    (do options))))

; ~~pattern matching things
; @TODO

; ---
; Macro definitions
; ---

; macro are basically bindings for the ast-* functions
(define letrec (macro ast-letrec))
(define let* (macro ast-let*))

(define cond (macro ast-cond))
