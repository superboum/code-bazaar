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

; ---
; Boolean logic
; ---
(define and (lambda (x y) (if x y nil)))
(define or (lambda (x y) (if x t y)))

; ---
; List manipulation
; ---
(define null? (lambda (maybe_lst) (eq maybe_lst nil)))
(define map (lambda (fn lst)
  (if lst (cons (fn (car lst)) (map fn (cdr lst))) nil)))

(define fold-left (lambda (fn acc lst)
  (if lst (fold-left fn (fn acc (car lst)) (cdr lst)) acc)))

; ---
; AST manipulation (useful for macros)
; ---
(define ast-lambda (lambda (args body) 
  (cons (quote lambda) (cons args (cons body nil)))))

(define ast-y (lambda (name body) 
  (cons (quote Y) (cons (ast-lambda (cons name nil) body) nil))))

(define ast-let (lambda (bind body)
  (cons 
    (quote let) 
    (cons 
      (cons (car bind) (cons (cadr bind) nil)) 
      (cons body nil)))))

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

; ---
; Macro definitions
; ---

; around let
(define letrec (macro ast-letrec))
(define let* (macro ast-let*))
