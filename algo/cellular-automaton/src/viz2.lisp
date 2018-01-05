(in-package :cellular-automaton)

(defun draw (renderer cell-list cell-size width height)
  ; Background
  (sdl2:set-render-draw-color renderer 100 100 100 255)
  (sdl2:render-clear renderer)

  ; Cells
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (map
    'list
    (lambda (x) (sdl2:render-fill-rect renderer (sdl2:make-rect
                  (+ (/ width 2) (* (second x) cell-size))
                  (+ (* (first x) cell-size) (/ height 2))
                  cell-size
                  cell-size
    ))) cell-list)
)

(defun main ()
  (let* ((config-pointer 0)
         (cell-size 5)
         (width 1700)
         (height 900)
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
         (current (first configs)))

    (sdl2:with-init (:everything)
      (sdl2:with-window (win
                          :title "Cellular Automaton"
                          :flags '(:shown)
                          :w width
                          :h height)
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (sdl2:with-event-loop (:method :poll)
            (:keyup
              (:keysym keysym)
              (cond
                ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)  (sdl2:push-event :quit))
                ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
                  (setq config-pointer (mod (+ config-pointer 1) (length configs)))
                  (setq current (nth config-pointer configs))
                )
            ))
            (:idle ()
              ; Blit
              (draw renderer (funcall (automaton-cell-list current) current) cell-size width height)
              (sdl2:render-present renderer)

              ; Update
              (setq current (funcall (automaton-update current) current))
            )
            (:quit () t)
))))))
