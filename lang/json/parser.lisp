#| Recursive_descent_parser
 |
 | == BNF Grammar ==
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
 | == Predict set ==
 | value -> array                                    start_brack
 | value -> object                                   start_brace
 | value -> number                                   number
 | value -> string                                   string
 | value -> bool                                     bool
 | kv -> string colon value                          string
 | root -> json                                      start_brack, start_brace
 | json -> array                                     start_brack
 | json -> object                                    start_brace
 | array -> start_brack inside_array end_brack       start_brack
 | inside_array -> value next_array                  number, string, bool, start_brack, start_brace
 | inside_array -> epsilon                           end_brack
 | next_array -> comma value next_array              comma
 | next_array -> epsilon                             end_brack
 | object -> start_brace inside_object end_brace     start_brace
 | inside_object -> kv next_object                   string
 | inside_object -> epsilon                          end_brace
 | next_object -> comma kv next_object               comma
 | next_object -> epsilon                            end_brace
 |
 |#

(defstruct parser
  :tokens
  :ast
)

(defun get-token (p) (first (parser-tokens p)))
(defun is-sym (p &rest symbols) (some (lambda (s) (eq s (first (get-token p)))) symbols))
(defun get-ast (p) (parser-ast p))
(defun set-ast (p a) (make-parser :tokens (parser-tokens p) :ast a))
(defun eat-token (p s)
  (cond
    ((is-sym p s) (make-parser :tokens (rest (parser-tokens p)) :ast (parser-ast p)))
    (t (error "Expecting ~S, got ~S instead" s (get-token p)))
))
(defun to-list-ast (p) (make-parser :tokens (parser-tokens p) :ast (list (parser-ast p))))
(defun cons-ast (np op) (make-parser :tokens (parser-tokens np) :ast (cons (parser-ast np) (parser-ast op))))
(defun reverse-ast (p) (make-parser :tokens (parser-tokens p) :ast (reverse (parser-ast p))))
(defun tag-ast (p tag) (make-parser :tokens (parser-tokens p) :ast (list tag (parser-ast p))))

(defun value (p)
  (tag-ast
    (cond
     ((is-sym p 'start_brack) (parray p))
     ((is-sym p 'start_brace) (object p))
     ((is-sym p 'number) (eat-token (set-ast p (get-token p)) 'number))
     ((is-sym p 'string) (eat-token (set-ast p (get-token p)) 'string))
     ((is-sym p 'bool) (eat-token (set-ast p (get-token p)) 'bool)))
   'value
))

(defun kv (p)
  (tag-ast
    (let* ((np (eat-token (eat-token (tag-ast (set-ast p (get-token p)) 'key) 'string) 'colon)))
      (cons-ast (value (set-ast np nil)) (to-list-ast np)))
    'kv
))

(defun parray (p)
  (tag-ast (eat-token (reverse-ast (inside-array (eat-token p 'start_brack))) 'end_brack) 'array)
)

(defun inside-array (p)
  (cond
    ((is-sym p 'number 'string 'bool 'start_brace 'start_brack) (next-array (to-list-ast (value p))))
    (t p)
))

(defun next-array (p)
  (cond
    ((is-sym p 'comma) (next-array (cons-ast (value (eat-token p 'comma)) p)))
    (t p)
))

(defun object (p)
  (tag-ast (eat-token (reverse-ast (inside-object (eat-token p 'start_brace))) 'end_brace) 'object)
)

(defun inside-object (p)
  (cond
    ((is-sym p 'string) (next-object (to-list-ast (kv p))))
    (t p)
))

(defun next-object (p)
  (cond
    ((is-sym p 'comma) (next-object (cons-ast (kv (eat-token p 'comma)) p)))
    (t p)
))

(defun json (p)
  (tag-ast
    (cond
      ((is-sym p 'start_brace) (object p))
      ((is-sym p 'start_brack) (parray p)))
    'json
))

(defun parse-json (tok)
  (parser-ast (json (make-parser :tokens tok :ast nil)))
)

;(print (json (make-parser
                ;:tokens '((start_brace) (string "key") (colon) (string "value") (comma) (string "n") (colon) (number 3) (end_brace) (the-end))
                ;:ast nil
             ;)))
;(print (parse-json '((start_brace) (string "symbols") (colon) (start_brack) (end_brack) (end_brace))))
