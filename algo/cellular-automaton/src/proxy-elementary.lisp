(in-package :cellular-automaton)

;; Elementary Proxy
(defstruct elem width cell-list line rule counter top)

(defun cell-list-elementary (a) (elem-cell-list (automaton-inter a)))

(defun next-elementary (a)
  (let* (
      (ea (automaton-inter a))
      (new-line (next-line (elem-line ea) (elem-rule ea)))
    )
    (make-automaton
      :inter (make-elem
        :width (elem-width ea)
        :cell-list (append
                     (elem-cell-list ea)
                     (convert-to-2d
                       new-line
                       (+ 1 (elem-top ea) (elem-counter ea))
                       (elem-width ea)))
        :line new-line
        :rule (elem-rule ea)
        :top (elem-top ea)
        :counter (+ 1 (elem-counter ea))
      )
      :update #'next-elementary
      :cell-list #'cell-list-elementary
)))

(defun create-elementary (r to w p)
  (let ((new-line (create-line w p)))
    (make-automaton
      :inter (make-elem
        :width w
        :top to
        :cell-list (convert-to-2d new-line to w)
        :line new-line
        :counter 0
        :rule (rule r)
      )
      :update #'next-elementary
      :cell-list #'cell-list-elementary
)))
