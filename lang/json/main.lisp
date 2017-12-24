(load "./lexer.lisp")
(load "./parser.lisp")

(print (parse-json (read-next *standard-input*)))
