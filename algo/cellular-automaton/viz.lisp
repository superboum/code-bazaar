(ql:quickload :sketch)
(load "life.lisp")

(defpackage :cellular-automaton (:use :cl :sketch :life))
(in-package :cellular-automaton)

(defsketch cellular-automaton
  ((title "Cellular Automaton")
   (width 1700)
   (height 900)
   (cell-size 5)
   (configs '(
     "config/spacefiller1_106.lif"
     "config/puffer.lif"
     "config/gosperglidergun_106.lif"
     "config/bigun_106.lif"
     ;"config/3enginecordershiprake_106.lif" ;; too slow
     "config/backrake1_106.lif"
   ))
   (config-pointer 0)
   (cell-list '())
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

(defmethod kit.sdl2:mousebutton-event ((window cellular-automaton) state ts b x y)
  (cond
    ((eq state :mousebuttondown)
      (with-slots (cell-list configs config-pointer) window
        (setq cell-list (with-open-file (stream (nth config-pointer configs)) (parse-life106 stream)))
        (setq config-pointer (mod (+ config-pointer 1) (length configs)))
))))

(make-instance 'cellular-automaton)
