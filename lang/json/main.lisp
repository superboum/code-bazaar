(load "./lexer.lisp")
(load "./parser.lisp")
(load "./to-s-expr.lisp")

(print (tos-json (build-ast (read-next *standard-input*))))
