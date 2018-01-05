(push (make-pathname :directory (pathname-directory *load-truename*)) asdf:*central-registry*)
(asdf:load-system 'cellular-automaton)
(cellular-automaton:main)
