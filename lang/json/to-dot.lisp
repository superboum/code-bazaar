; dot ./test.dot -Tpng -O
; twurl "/1.1/statuses/show.json?id=938107501429559297" | sbcl --script ./main.lisp | dot -Tsvg -o graph.svg

(defun tod-is-sym (a s) (eq (first a) s))

(defun tod-expect (a s)
  (cond
    ((tod-is-sym a s) (second a))
    (t (error "Expecting ~s, got ~s" s (first a)))
))

(defun tod-json (a parent counter)
  (cons (list counter "json" parent)
    (let* ((next (tod-expect a 'json)))
      (cond
        ((tod-is-sym next 'array) (tod-array next parent (+ counter 1)))
        ((tod-is-sym next 'object) (tod-object next parent (+ counter 1)))
))))

(defun tod-array (a parent counter)
  (let*
    ((local-parent counter)
     (values (tod-expect a 'array))
     (entries
       (reduce
         (lambda (acc v)
           (let* ((entries (tod-values v local-parent (second acc))))
             (list (append (first acc) entries) (+ (length entries) (second acc)))
         ))
         values
         :initial-value (list nil (+ counter 1)))
     ))
     (cons (list counter "array" parent) (first entries))
))

(defun tod-object (a parent counter)
  (let*
    ((local-parent counter)
     (values (tod-expect a 'object))
     (entries
       (reduce
         (lambda (acc v)
           (let* ((entries (tod-kv v local-parent (second acc))))
             (list (append (first acc) entries) (+ (length entries) (second acc)))
         ))
         values
         :initial-value (list nil (+ counter 1)))
     ))
     (cons (list counter "object" parent) (first entries))
))

(defun tod-kv (a parent counter)
  (let*
    ((values (tod-expect a 'kv))
     (key (tod-key (assoc 'key values) parent counter))
     (value (tod-values (assoc 'value values) counter (+ 1 counter))))
    (cons key value)
))

(defun tod-key (a parent counter) (list counter (tod-expect (tod-expect a 'key) 'string) parent))

(defun tod-values (a parent counter)
  (let* ((next (tod-expect a 'value)))
    (cond
      ((tod-is-sym next 'number) (tod-number next parent counter))
      ((tod-is-sym next 'string) (tod-string next parent counter))
      ((tod-is-sym next 'bool) (tod-bool next parent counter))
      ((tod-is-sym next 'object) (tod-object next parent counter))
      ((tod-is-sym next 'array) (tod-array next parent counter))
)))

(defun tod-number (a parent counter) (list (list counter (tod-expect a 'number) parent)))
(defun tod-string (a parent counter) (list (list counter (tod-expect a 'string) parent)))
(defun tod-bool (a parent counter)
  (list (list counter (cond ((tod-expect a 'bool) "true") (t "false")) parent)))

(defun tod-template-nodes (out)
  (reduce
    (lambda (acc node)
      (concatenate
        'string
        acc
        (format nil "node~a[label=\"~a\"];~%" (first node) (remove #\" (format nil "~a" (second node))))))
    out
    :initial-value ""
))

(defun tod-template-links (out)
  (reduce
    (lambda (acc node)
      (cond
        ((eq (first node) (third node)) acc)
        (t
          (concatenate
            'string
            acc
            (format nil "node~a -> node~a;~%" (third node) (first node))))))
    out
    :initial-value ""
))

(defun tod-template (out)
  (format
    nil
    "digraph json {~%rankdir=\"LR\";node[shape=plaintext];~%~a~%~a}~%"
    (tod-template-nodes out)
    (tod-template-links out)
))

;(format t "~a" (tod-template (tod-json '(JSON (OBJECT ((KV ((VALUE (NUMBER 6)) (KEY (STRING "toto"))))))) 0 0)))
