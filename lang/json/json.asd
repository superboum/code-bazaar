(asdf:defsystem #:json
  :pathname "./"
  :serial t
  :components ((:file "foundation")
               (:file "lexer")
               (:file "parser")
               (:file "to-dot")
               (:file "to-s-expr")
))
