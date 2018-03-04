(ql:quickload "cxml")

(defparameter *graph* (make-hash-table :test 'equal))

(defun add-undirected-edge (g vert1 vert2)
  (setf (gethash vert1 g) (cons vert2 (gethash vert1 g)))
  (setf (gethash vert2 g) (cons vert1 (gethash vert2 g))))

(defun print-hash-entry (key value)
  (format t "~S: ~S~%" key value))

(defun get-ways (sexpr)
  (remove-if-not
    (lambda (x) (and
      (listp x)
      (stringp (first x))
      (string= (first x) "way")
    ))
    sexpr))

(defun find-objs (obj sexpr)
  (remove-if-not
    (lambda (x)
      (and
        (listp x)
        (stringp (first x))
        (string= (first x) obj)
    ))
    sexpr))
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
    (get-ways sexpr)))

(defun extract-paths (sexpr)
  (mapcar
    (lambda (x)
      (mapcar (lambda (y) (second (get-nd-ref y))) (find-nds x)))
   sexpr))

(defun add-path (g path-list)
  (cond
    ((null (second path-list)) nil)
    (t (add-undirected-edge g (first path-list) (second path-list)) (add-path g (rest path-list)))))

(defun add-paths (g path-lists)
  (reduce (lambda (y x) (add-path g x)) path-lists))

(add-paths *graph* (extract-paths (search-footway (cxml:parse-file "map.osm" (cxml-xmls:make-xmls-builder)))))
(maphash #'print-hash-entry *graph*)

