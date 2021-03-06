(in-package :cellular-automaton)

(defsketch cellular-automaton
  ; Setup
  ((title "Cellular Automaton")
   (width 1700)
   (height 900)
   (cell-size 5)
   (configs
     (append
       (list
         (create-lif "config/puffer.lif")
         (create-lif "config/gosperglidergun_106.lif")
         (create-lif "config/bigun_106.lif")
         (create-lif "config/backrake1_106.lif")
         (create-lif "config/spacefiller1_106.lif")
       )
       (loop for x from 0 to 255 collect
         (create-elementary x (/ height -2 cell-size) 401 '(200))
   )))
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
    (funcall (automaton-cell-list current) current))

  ; Update
  (setq current (funcall (automaton-update current) current))
)

(defmethod kit.sdl2:mousebutton-event ((window cellular-automaton) state ts b x y)
  (cond
    ((eq state :mousebuttondown)
      (with-slots (current configs config-pointer) window
        (setq config-pointer (mod (+ config-pointer 1) (length configs)))
        (setq current (nth config-pointer configs))
))))
