(push (make-pathname :directory (pathname-directory *load-truename*)) asdf:*central-registry*)
(asdf:load-system 'json)

;(format t "~a" (tod-template (tod-json (build-ast (lex *standard-input*)) 0 0)))
(format t "~a" (tos-json (build-ast (lex *standard-input*))))
