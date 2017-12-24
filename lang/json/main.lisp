(load "./lexer.lisp")
(load "./parser.lisp")
(load "./to-s-expr.lisp")

(print (tos-json (parse-json (read-next *standard-input*))))
