(in-package :cellular-automaton)

; Constant. reversed to fit rule fun
(defconstant patterns '((nil nil nil) (nil nil t) (nil t nil) (nil t t) (t nil nil) (t nil t) (t t nil) (t t t)))

; Reversed. Example 8d = 00010000 = '(nil nil nil t nil nil nil nil) = '(nil nil nil t)
(defun rule (n)
  (cond
    ((< n 1) '())
    ((= n 1) '(t))
    ((zerop (mod n 2)) (cons nil (rule (/ n 2))))
    (t (cons t (rule (floor n 2))))
))

(defun next (tri r)
  (nth (position tri patterns :test 'equal) r)
)

(defun at-least (l n)
  (cond
    ((null l) nil)
    ((<= n 1) t)
    (t (at-least (rest l) (- n 1)))
))

(defun next-line (line rule)
  (labels ((rec (l r)
    (cond
      ((at-least l 3) (cons (next (subseq l 0 3) r) (rec (rest l) r)))
      (t nil)
  )))
  (append '(nil) (rec line rule) '(nil))
))

(defun create-line (width pos)
  (labels ((rec (counter p)
    (cond
      ((eq counter width) nil)
      ((eq counter (first p)) (cons t (rec (+ 1 counter) (rest p))))
      (t (cons nil (rec (+ 1 counter) p)))
    )))
  (rec 0 pos)
))

(defun convert-to-2d (line curr-height max-width)
  (labels ((rec (pos l)
    (cond
      ((null l) nil)
      ((first l) (cons (list curr-height pos) (rec (+ 1 pos) (rest l))))
      (t (rec (+ 1 pos) (rest l)))
  )))
  (rec (ceiling max-width -2) line)
))
