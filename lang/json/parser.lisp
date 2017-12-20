#|
 | value ::= array | object | number | string | bool
 | kv ::= string ":" value
 |
 | root ::= json
 | json ::= array | object
 |
 | array ::= "[" inside_array "]"
 | inside_array ::= value next_array | ε
 | next_array ::= "," value next_array | ε
 |
 | object ::= "{" inside_object "}"
 | inside_object ::= kv next_object | ε
 | next_object ::= "," kv next_object | ε
 |
 |#

(defun curry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))
))

(defun accept (s tokens)
  (cond
    ((eq (first (first tokens)) s) (rest tokens)) ; Always have at least one token: the_end
    (t nil)
))

(defun expect (s tokens)
  (let* ((res (accept s tokens)))
    (cond
      (res res)
      (t (error "Expect failed on token ~S" s))
)))

(defun one-choice (tokens x)
  (let* ((res (accept (first x) tokens)))
    (cond
      (res (funcall (second x) res))
      (t nil)
)))

(defun choose (l tokens &key fail)
  (let* ((res (some (curry #'one-choice tokens) l)))
    (cond
      (res res)
      (fail (error "Choose failed on token ~S" (first (first tokens))))
      (t nil)
)))

(defun all (l tokens)
  (reduce (lambda (acc x) (funcall x acc)) l :initial-value tokens)
)

(defun nothing (x) x)

(defun try-value (tokens)
  (choose
    (list
      (list 'number #'nothing)
      (list 'string #'nothing)
      (list 'bool #'nothing)
      (list 'start_brack #'parray)
      (list 'start_brace #'object))
      tokens
))

(defun value (tokens)
  (let* ((res (try-value tokens)))
    (cond
      (res res)
      (t (error "Value failed on token ~S" (first (first tokens))))
)))

(defun kv (tokens)
  (all
    (list
      (curry #'expect 'string)
      (curry #'expect 'colon)
      #'value)
    tokens
))

(defun next-array (tokens)
  (let* ((comma (accept 'comma tokens)))
    (cond
      (comma (next-array (value comma)))
      (t tokens)
)))

(defun inside-array (tokens)
  (let* ((val (try-value tokens)))
    (cond
      (val (next-array val))
      (t tokens)
)))

(defun parray (tokens)
  (all
    (list
      #'inside-array
      (curry #'expect 'end_brack))
    tokens
))

(defun next-object (tokens)
  (let* ((comma (accept 'comma tokens)))
    (cond
      (comma (next-object (kv comma)))
      (t tokens)
)))

(defun inside-object (tokens)
  (let* ((val (accept 'string tokens)))
    (cond
      (val (next-object (kv tokens)))
      (t tokens)
)))

(defun object (tokens)
  (all
    (list
      #'inside-object
      (curry #'expect 'end_brace))
    tokens
))

(defun json (tokens)
  (choose
    (list
     (list 'start_brack #'parray)
     (list 'start_brace #'object))
    tokens
    :fail t
))

;(print (kv '((string) (colon) (number) (the-end))))
;(print (json '((start_brack) (number) (comma) (number) (end_brack) (the-end))))
(print (json '((start_brace) (string) (colon) (number) (comma) (string) (colon) (string) (end_brace) (the-end))))
;(print (accept 'string '((string) (colon) (number))))
