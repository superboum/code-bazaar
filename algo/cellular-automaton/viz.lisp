(ql:quickload :sketch)
(load "life.lisp")

(defpackage :cellular-automaton (:use :cl :sketch :life))
(in-package :cellular-automaton)

(defsketch cellular-automaton
  ((title "Cellular Automaton")
   (width 1700)
   (height 900)
   (cell-size 5)
   (config "config/spacefiller1_106.lif")
   (cell-list (with-open-file (stream config) (parse-life106 stream)))
  )
  (map
    'list
    (lambda (x) (rect
                  (+ (/ width 2) (* (second x) cell-size))
                  (+ (* (first x) cell-size) (/ height 2))
                  cell-size
                  cell-size
    ))
    cell-list)
  (setq cell-list (one-turn cell-list))
)

(make-instance 'cellular-automaton)
