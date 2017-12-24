(load "./lexer.lisp")
(load "./parser.lisp")
(load "./to-s-expr.lisp")
(load "./to-dot.lisp")

(format t "~a" (tod-template (tod-json (build-ast (lex *standard-input*)) 0 0)))
