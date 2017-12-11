(ql:quickload :sketch)
(load "life.lisp")
(load "elementary.lisp")

(defpackage :cellular-automaton (:use :cl :sketch :life :elementary))
(in-package :cellular-automaton)

(defsketch cellular-automaton
  ; Setup
  ((title "Cellular Automaton")
   (width 1700)
   (height 900)
   (cell-size 5)
   (configs
     (append
       (loop for x from 0 to 255 collect
        (list
          (create-elementary x (/ height -2 cell-size) 401 '(200))
          #'next-elementary
          #'cell-list-elementary
   ))))
   (config-pointer 0)
   (current (first configs))
  )

  ; Draw
  (map
    'list
    (lambda (x) (rect
                  (+ (/ width 2) (* (second x) cell-size))
                  (+ (* (first x) cell-size) (/ height 2))
                  cell-size
                  cell-size
    ))
    (funcall (third current) (first current)))

  ; Update
  (setq current (cons (funcall (second current) (first current)) (rest current)))
)

(defmethod kit.sdl2:mousebutton-event ((window cellular-automaton) state ts b x y)
  (cond
    ((eq state :mousebuttondown)
      (with-slots (current configs config-pointer) window
        (setq current (nth config-pointer configs))
        (setq config-pointer (mod (+ config-pointer 1) (length configs)))
))))

(make-instance 'cellular-automaton)
