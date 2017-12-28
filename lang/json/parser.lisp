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

; Simple parser structure
(defstruct parser
  :tokens
  :ast
)

; Token handling
(defun get-token (p) (first (parser-tokens p)))
(defun is-sym (p &rest symbols)
  (some
    (lambda (s)
      (pipeline (v p)
        (get-token v) ; Get next token from PARSER:TOKENS
        (first v)     ; Extract symbol from token
        (eq s v)))    ; Check if passed symbol is equal to extracted symbol
    symbols))
(defun eat-token (p s)
  (cond
    ((is-sym p s) (make-parser :tokens (rest (parser-tokens p)) :ast (parser-ast p)))
    (t (error "Expecting ~S, got ~S instead" s (get-token p)))
))

; AST getters and setters
(defun get-ast (p) (parser-ast p))
(defun set-ast (p a) (make-parser :tokens (parser-tokens p) :ast a))

; AST transformations
(defun to-list-ast (p) (make-parser :tokens (parser-tokens p) :ast (list (parser-ast p))))
(defun cons-ast (np op) (make-parser :tokens (parser-tokens np) :ast (cons (parser-ast np) (parser-ast op))))
(defun reverse-ast (p) (make-parser :tokens (parser-tokens p) :ast (reverse (parser-ast p))))
(defun tag-ast (p tag) (make-parser :tokens (parser-tokens p) :ast (list tag (parser-ast p))))

; Implement grammar
(defun value (p)
  (labels

    ((process-terminal (p type)
       (pipeline (v p)
         (get-token v)         ; Get next token in PARSER:TOKENS
         (set-ast p v)         ; Set PARSER:AST with this token
         (eat-token v type)))) ; Eat this token (remove this token from PARSER:TOKENS)

    (tag-ast
      (cond
        ((is-sym p 'start_brack) (parray p))
        ((is-sym p 'start_brace) (object p))
        ((is-sym p 'number) (process-terminal p 'number))
        ((is-sym p 'string) (process-terminal p 'string))
        ((is-sym p 'bool) (process-terminal p 'bool)))
      'value)))

(defun kv (p)
  (tag-ast
    (let*

      ((np (pipeline (v p)
             (get-token v)
             (set-ast p v)
             (tag-ast v 'key)
             (eat-token v 'string)
             (eat-token v 'colon))))

      (pipeline (v np)
        (set-ast v nil)
        (value v)
        (cons-ast v (to-list-ast np))))

    'kv))

(defun parray (p)
  (pipeline (v p)
    (eat-token v 'start_brack)
    (inside-array v)
    (reverse-ast v)
    (eat-token v 'end_brack)
    (tag-ast v 'array)))

(defun inside-array (p)
  (cond
    ((is-sym p 'number 'string 'bool 'start_brace 'start_brack)
     (pipeline (v p)
       (value v)
       (to-list-ast v)
       (next-array v)))
    (t p)))

(defun next-array (p)
  (cond
    ((is-sym p 'comma)
     (pipeline (v p)
       (eat-token v 'comma)
       (value v)
       (cons-ast v p)
       (next-array v)))
    (t p)))

(defun object (p)
  (pipeline (v p)
    (eat-token v 'start_brace)
    (inside-object v)
    (reverse-ast v)
    (eat-token v 'end_brace)
    (tag-ast v 'object)))

(defun inside-object (p)
  (cond
    ((is-sym p 'string)
     (pipeline (v p)
       (kv v)
       (to-list-ast v)
       (next-object v)))
    (t p)))

(defun next-object (p)
  (cond
    ((is-sym p 'comma)
     (pipeline (v p)
       (eat-token v 'comma)
       (kv v)
       (cons-ast v p)
       (next-object v)))
    (t p)))

(defun json (p)
  (tag-ast
    (cond
      ((is-sym p 'start_brace) (object p))
      ((is-sym p 'start_brack) (parray p)))
    'json))

(defun build-ast (tok)
  (pipeline (v tok)
    (make-parser :tokens v :ast nil)
    (json v)
    (parser-ast v)))

;(print (json (make-parser
                ;:tokens '((start_brace) (string "key") (colon) (string "value") (comma) (string "n") (colon) (number 3) (end_brace) (the-end))
                ;:ast nil
             ;)))
;(print (parse-json '((start_brace) (string "symbols") (colon) (start_brack) (end_brack) (end_brace))))
