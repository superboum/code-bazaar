(ql:quickload "cxml")
(ql:quickload "sdl2")

;;;;;;;;;;;;;;;;;;;
; UTILS
;;;;;;;;;;;;;;;;;;;
(defun parse-number (str)
  (with-input-from-string (in str) (read in)))

(defun mapval (v smin smax dmin dmax)
  (+ (* (/ (- v smin) (- smax smin)) (- dmax dmin)) dmin))

;;;;;;;;;;;;;;;;;;;
; GRAPH
;;;;;;;;;;;;;;;;;;;

(defun add-undirected-edge (g vert1 vert2)
  (setf (gethash vert1 g) (cons vert2 (gethash vert1 g)))
  (setf (gethash vert2 g) (cons vert1 (gethash vert2 g))))

(defun add-path (g path-list)
  (cond
    ((null (second path-list)) nil)
    (t (add-undirected-edge g (first path-list) (second path-list)) (add-path g (rest path-list)))))

(defun add-paths (g path-lists)
  (reduce (lambda (y x) (add-path g x) y) path-lists)
  g)

(defun print-hash-entry (key value)
  (format t "~S: ~S~%" key value))

;;;;;;;;;;;;;;;;;;;
; EXTRACT DATA FROM OSM FILE
;;;;;;;;;;;;;;;;;;;

(defun find-objs (obj sexpr)
  (remove-if-not
    (lambda (x)
      (and
        (listp x)
        (stringp (first x))
        (string= (first x) obj)
    ))
    sexpr))
(defun find-ways (sexpr) (find-objs "way" sexpr))
(defun find-bounds (sexpr) (find-objs "bounds" sexpr))
(defun find-nodes (sexpr) (find-objs "node" sexpr))
(defun find-tags (sexpr) (find-objs "tag" sexpr))
(defun find-nds (sexpr) (find-objs "nd" sexpr))

(defun get-property (prop content sexpr)
  (find-if
    (lambda (x) (and (string= (first x) prop) (or (null content) (string= (second x) content))))
    (second sexpr)))
(defun get-tag-key (key sexpr) (get-property "k" key sexpr))
(defun get-tag-value (value sexpr) (get-property "v" value sexpr))
(defun get-nd-ref (sexpr) (get-property "ref" nil sexpr))

(defun find-tag-key (key sexpr)
  (find-if (lambda (x) (get-tag-key key x)) sexpr))

(defun search-footway (sexpr)
  (remove-if-not
    (lambda (x)
      (get-tag-value
	"footway"
        (find-tag-key
          "highway"
          (find-tags x))))
    (find-ways sexpr)))

(defun extract-paths (sexpr)
  (mapcar
    (lambda (x)
      (mapcar (lambda (y) (second (get-nd-ref y))) (find-nds x)))
   sexpr))

(defun get-bounds (sexpr)
  (mapcar
    (lambda (x)
      (parse-number (second (get-property x nil (first sexpr)))))
    '("maxlon" "maxlat" "minlon" "minlat")))

(defun extract-nodes (ht sexpr)
  (mapcar
    (lambda (node)
      (setf
	(gethash (second (get-property "id" nil node)) ht)
	(mapcar (lambda (x) (parse-number (second (get-property x nil node)))) '("lon" "lat"))))
    (find-nodes sexpr))
  ht)

;;;;;;;;;;;;;;;;;;;
; MAIN
;;;;;;;;;;;;;;;;;;;

(let*
  ((width 1700)
   (height 1700)
   (sdata (cxml:parse-file "map.osm" (cxml-xmls:make-xmls-builder)))
   (bounds (get-bounds (find-bounds sdata)))
   (nodes (extract-nodes (make-hash-table :test 'equal) sdata))
   (full-graph (add-paths (make-hash-table :test 'equal) (extract-paths (search-footway sdata)))))

  ;(maphash #'print-hash-entry nodes)
  (defun compute-position (id)
    (let*
      ((nodeinfo (gethash id nodes))
       (lon (first nodeinfo))
       (lat (second nodeinfo))
       (pixw (round (mapval lon (third bounds) (first bounds) 100 (- width 100))))
       (pixh (round (mapval lat (second bounds) (fourth bounds) 100 (- height 100)))))
      (values pixw pixh)))

  (defun draw-edges (renderer my-id neighbours-id)
      (sdl2:set-render-draw-color renderer 255 0 0 255)
      (multiple-value-bind
	(psrcw psrch)
	(compute-position my-id)
        (mapcar
	  (lambda (id)
	    (multiple-value-bind
	      (pdestw pdesth)
	      (compute-position id)
              (sdl2:render-draw-line renderer psrcw psrch pdestw pdesth)))
          (remove-if (lambda (id) (string<= id my-id)) neighbours-id))))

  (defun draw-node (renderer id)
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (multiple-value-bind
      (pixw pixh)
      (compute-position id)
      (sdl2:render-fill-rect
        renderer
        (sdl2:make-rect pixw pixh 2 2))))

  (defun draw (renderer)
    (sdl2:set-render-draw-color renderer 100 100 100 255)
    (sdl2:render-clear renderer)

    (maphash
      (lambda (k v)
	(draw-node renderer k)
	(draw-edges renderer k v))
      full-graph))
  
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Graph Show" :flags '(:shown) :w width :h height)
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)
	  (:idle ()
            (draw renderer)
	    (sdl2:render-present renderer))
          (:keyup
            (:keysym keysym)
            (cond
              ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)  (sdl2:push-event :quit))))
  ))))

  (exit)
)


