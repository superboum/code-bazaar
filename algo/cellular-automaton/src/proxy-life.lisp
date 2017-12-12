(in-package :cellular-automaton)

(defstruct lif cell-list counter)

(defun cell-list-lif (a) (lif-cell-list (automaton-inter a)))

(defun next-lif (a)
  (let* ((l (automaton-inter a)))
    (make-automaton
      :inter (make-lif
               :cell-list (one-turn (lif-cell-list l))
               :counter (+ 1 (lif-counter l)))
      :update #'next-lif
      :cell-list #'cell-list-lif
)))

(defun create-lif (f)
  (make-automaton
    :inter (make-lif
             :cell-list (with-open-file (stream f) (parse-life106 stream))
             :counter 0)
    :update #'next-lif
    :cell-list #'cell-list-lif
))
