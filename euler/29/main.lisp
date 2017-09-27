(defun gen-seq (current-a current-b max-a max-b)
  (cond
    ((> current-a max-a) '())
    ((> current-b max-b) (gen-seq (+ current-a 1) 2 max-a max-b))
    (t (cons (expt current-a current-b) (gen-seq current-a (+ current-b 1) max-a max-b)))
))

(print (length (remove-duplicates (gen-seq 2 2 100 100))))
