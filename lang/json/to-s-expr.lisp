(defun tos-is-sym (a s) (eq (first a) s))

(defun tos-expect (a s)
  (cond
    ((tos-is-sym a s) (second a))
    (t (error "Expecting ~s, got ~s" s (first a)))
))

(defun tos-json (a)
  (let* ((next (tos-expect a 'json)))
    (cond
      ((tos-is-sym next 'array) (tos-array next))
      ((tos-is-sym next 'object) (tos-object next))
)))

(defun tos-array (a)
  (let* ((values (tos-expect a 'array)))
    (map 'list #'tos-values values)
))

(defun tos-values (a)
  (let* ((next (tos-expect a 'value)))
    (cond
      ((tos-is-sym next 'number) (tos-number next))
      ((tos-is-sym next 'string) (tos-string next))
      ((tos-is-sym next 'bool) (tos-bool next))
      ((tos-is-sym next 'object) (tos-object next))
      ((tos-is-sym next 'array) (tos-array next))
)))

(defun tos-object (a)
  (let* ((values (tos-expect a 'object)))
    (map 'list #'tos-kv values)
))

(defun tos-key (a) (tos-expect (tos-expect a 'key) 'string))

(defun tos-kv (a)
  (let*
    ((values (tos-expect a 'kv))
     (key (tos-key (assoc 'key values)))
     (value (tos-values (assoc 'value values))))
    (cons key value)
))

(defun tos-number (a) (tos-expect a 'number))
(defun tos-string (a) (tos-expect a 'string))
(defun tos-bool (a) (tos-expect a 'bool))

;(print (tos-json '(JSON (OBJECT ((KV ((VALUE (NUMBER 6)) (KEY (STRING "toto")))))))))
