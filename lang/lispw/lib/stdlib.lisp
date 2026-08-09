; ---
; Recursivity
; ---
(define Y (lambda (f) 
  ((lambda (x) (f (x x))) 
   (lambda (x) (f (x x)))))

; ---
; Boolean logic
; ---
(define and (lambda (x y) (if x y nil)))
(define or (lambda (x y) (if x t y)))

; ---
; List manipulation
; ---

;@FIXME add map
;@FIXME add fold

; ---
; AST manipulation (useful for macros)
; ---
(define ast-lambda (lambda (args body) 
  (cons (quote lambda) (cons args (cons body nil)))))

(define ast-y (lambda (name body) 
  (cons (quote Y) (cons (ast-lambda (cons name nil) body) nil))))

(define ast-letrec (lambda (bind body) 
  (cons 
    (quote let) 
    (cons 
      (cons (car bind) (cons (ast-y (car bind) (cadr bind)) nil)) 
      (cons body nil)))))

; ---
; Macro definitions
; ---
(define letrec (macro ast-letrec))
