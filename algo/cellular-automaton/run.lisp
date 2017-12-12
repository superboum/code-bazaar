(push (make-pathname :directory (pathname-directory *load-truename*)) asdf:*central-registry*)
(asdf:load-system 'cellular-automaton)
(cellular-automaton:main)
; Broken for now
;(sb-ext:save-lisp-and-die "my_binary" :executable t :toplevel 'main)

