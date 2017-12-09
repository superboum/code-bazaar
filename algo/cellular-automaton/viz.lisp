(ql:quickload :sketch)
(load "life.lisp")

(defpackage :cellular-automaton (:use :cl :sketch :life))
(in-package :cellular-automaton)

(defsketch cellular-automaton
  ((title "Cellular Automaton")
   (width 1700)
   (height 900)
   (cell-list '((0 1) (1 0) (1 2) (3 0) (3 3) (4 2) (4 3) (5 3)))
  )
  (map 'list (lambda (x) (rect (+ (/ width 2) (* (second x) 10)) (+ (* (first x) 10) (/ height 2)) 10 10)) cell-list)
  (setq cell-list (one-turn cell-list))
)

(make-instance 'cellular-automaton)
