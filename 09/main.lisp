#!/usr/bin/sbcl --script
; a²+b² = c²
; a+b+c = 1000 <=> c = 1000-a-b
;
; a²+b² = (1000-a-b)²
; a²+b²-(1000-a-b)² = 0

(defun f (a b) (- (+ (* a a) (* b b)) (expt (- 1000 a b) 2)))

(defun find-vars (a b)
  (cond
    ((eq (f a b) 0) (list a b))
    ((eq b a) (find-vars (+ 1 a) 0))
    (t (find-vars a (+ 1 b)))
))

(let ((ab (find-vars 0 0)))
  (print (* (first ab) (second ab) (- 1000 (first ab) (second ab))))
)
