(push (make-pathname :directory (pathname-directory *load-truename*)) asdf:*central-registry*)
(asdf:load-system 'cellular-automaton)
(sb-ext:save-lisp-and-die "cellular_automaton" :executable t :toplevel 'cellular-automaton:main)
