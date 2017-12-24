(load "./lexer.lisp")
(load "./parser2.lisp")

(print (parse-json (read-next *standard-input*)))
