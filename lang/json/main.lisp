(load "./lexer.lisp")
(load "./parser.lisp")

(print (json (read-next *standard-input*)))
