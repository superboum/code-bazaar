; read-char, peek-char, unread-char

(defun parse-float (s)
  (with-input-from-string (in s)
    (read in)
))

(defun is-digit (s)
  (or (digit-char-p s) (eq #\. s) (eq #\- s))
)

(defun get-digit (start)
  (labels (
    (read-digit (s)
      (cond
        ((is-digit s) (cons s (read-digit (read-char))))
        (t (unread-char s) nil)
    ))
  )
  (list 'number (parse-float (coerce (read-digit start) 'string)))
))

(defun get-string ()
  (labels (
    (read-string ()
      (let* ((s (read-char)))
        (cond
          ((eq #\" s) nil)
          (t (cons s (read-string)))
    )))
  )
  (list 'string (coerce (read-string) 'string))
))

(defun check-keyword (reference start)
  (labels (
    (rec-check (ref acc)
      (let* ((s (read-char *standard-input* nil)))
        (cond
          ((null ref) (unread-char s) t)
          ((eq (first ref) s) (rec-check (rest ref) (cons s acc)))
          (t (reduce #'unread-char (cons s acc)) nil)
    )))
  )
  (unread-char start)
  (rec-check (coerce reference 'list) '())
))

(defun read-next ()
  (let* ((s (read-char *standard-input* nil)))
    (cond
      ((eq nil s) nil)
      ((some (lambda (x) (eq x s)) (list #\space #\linefeed #\return #\tab)) (read-next))
      ((is-digit s) (cons (get-digit s) (read-next)))
      ((eq #\" s) (cons (get-string) (read-next)))
      ((eq #\{ s) (cons (list 'start_brace) (read-next)))
      ((eq #\} s) (cons (list 'end_brace) (read-next)))
      ((eq #\[ s) (cons (list 'start_brack) (read-next)))
      ((eq #\] s) (cons (list 'end_brack) (read-next)))
      ((eq #\, s) (cons (list 'comma) (read-next)))
      ((eq #\: s) (cons (list 'colon) (read-next)))
      ((check-keyword "true" s) (cons (list 'true) (read-next)))
      ((check-keyword "false" s) (cons (list 'false) (read-next)))
      (t (list 'error s))
)))

(print (read-next))
