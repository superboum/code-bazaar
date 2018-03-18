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

(defun get-property (prop content sexpr)
  (find-if
    (lambda (x) (and (string= (first x) prop) (or (null content) (string= (second x) content))))
    (second sexpr)))

(defun find-tag-key (key sexpr)
  (find-if (lambda (x) (get-property "k" key x)) sexpr))

(defun search-footway (sexpr)
  (remove-if-not
    (lambda (x)
      (some
	(lambda (selected-highways) 
	  (second (get-property "v" selected-highways (find-tag-key "highway" (find-objs "tag" x)))))
	'("footway" "path")))
    (find-objs "way" sexpr)))

(defun search-way (name sexpr)
  (find-if
    (lambda (way)
      (get-property "v" name (find-tag-key "name" (find-objs "tag" way))))
    (find-objs "way" sexpr)))

(defun extract-path (sexpr)
  (mapcar (lambda (y) (second (get-property "ref" nil y))) (find-objs "nd" sexpr)))

(defun extract-paths (sexpr)
  (mapcar #'extract-path sexpr))

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
    (find-objs "node" sexpr))
  ht)


;;;;;;;;;;;;;;;;;;;
; RAY CASTING
; Source: https://rosettacode.org/wiki/Ray-casting_algorithm
;;;;;;;;;;;;;;;;;;;

(defun point-vorder (&rest points)
  (sort points (lambda (x y) (> (second x) (second y)))))

(defun prepare-point (p ax ay bx by &optional (epsilon .001))
  (destructuring-bind (px py) p
    (cond
      ((= py ay) (list px (- epsilon py)))
      ((= py by) (list px (+ epsilon py)))
      (t (list px py)))))

(defun ray-intersects-segment (p a b)
  (destructuring-bind ((ax ay) (bx by)) (point-vorder a b)
    (destructuring-bind (px py) (prepare-point p ax ay  bx by)
      (cond
	;; Point is below the rectangle -> no intersection
	((> py ay) nil)
	;; Point is above the rectangle -> no intersection
	((< py by) nil)
	;; Point is to the right of the rectangle -> no intersection
	((> px (max ax bx)) nil)
	;; Point is to the left of the rectangle -> intersection
	((< px (min ax bx)) t)
	;; Point is within the rectangle -> more work :'(
	(t
	  (let
	    ((angle-segment (if (= ax bx) nil (/ (- by ay) (- bx ax))))
	     (angle-point (if (= ax px) nil (/ (- py ay) (- px ax)))))

	    (cond
	      ;; Rectangle width is 0, so this is a segment, so if here in the code, our point in on the segment -> intersection
	      ((null angle-segment) t)
              ;; ax is smaller than bx, so if our point create a vertical segment, it is on the left side of the rectangle -> intersection
	      ((and (< ax bx) (null angle-point)) t)
	      ;; otherwise our point create a vertical segment on the right side of the rectangle -> no intersection
	      ((null angle-point) nil)
	      ;; general case
	      (t (>= angle-point angle-segment))
)))))))


(defun sides-rec (polygon last-point) 
  (cond
    ((null polygon) nil)
    (t (cons (list last-point (first polygon)) (sides-rec (rest polygon) (first polygon))))))

(defun sides (polygon)
  (sides-rec polygon (first (last polygon))))

(defun inside-polygon (p polygon)
  (reduce
    (lambda (acc side) (+ acc (if (ray-intersects-segment p (first side) (second side)) 1 0))) 
    (sides polygon)
    :initial-value 0))

(defun maphash2 (fn ht)
  (let 
    ((res '()))
    (maphash (lambda (key value) (push (funcall fn key value) res)) ht)
    res))

(defun restrict-to-polygon (g polygon nodes)
  (maphash2 (lambda (key value) (inside-polygon (gethash key nodes) polygon)) g))

;;;;;;;;;;;;;;;;;;;
; MAIN
;;;;;;;;;;;;;;;;;;;

(let*
  ((width 1700)
   (height 1700)
   (sdata (cxml:parse-file "map.osm" (cxml-xmls:make-xmls-builder)))
   (bounds (get-bounds (find-objs "bounds" sdata)))
   (nodes (extract-nodes (make-hash-table :test 'equal) sdata))
   (region (search-way "Parc des Gayeulles" sdata)) 
   (region-polygon (mapcar (lambda (x) (gethash x nodes)) (extract-path region)))
   (full-graph (add-paths (make-hash-table :test 'equal) (extract-paths (search-footway sdata)))))

  (print (restrict-to-polygon full-graph region-polygon nodes))
  (exit)
  (print (sides region-polygon))
  (print (ray-intersects-segment '(2 5) '(3 3) '(3 10)))

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
    (sdl2:set-render-draw-color renderer 20 20 20 255)
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


