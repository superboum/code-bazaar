; read-char, peek-char, unread-char

(defun parse-float (s)
  (with-input-from-string (in s)
    (read in)
))

(defun is-digit (stream)
  (let* ((s (peek-char nil stream nil)))
    (cond
      ((or (digit-char-p s) (eq #\. s) (eq #\- s)) t)
      (t nil)
)))

(defun get-digit (stream)
  (labels (
    (read-digit ()
      (cond
        ((is-digit stream) (cons (read-char stream) (read-digit)))
        (t nil)
    ))
  )
  (list 'number (parse-float (coerce (read-digit) 'string)))
))

(defun get-string (stream)
  (labels (
    (read-string (escape)
      (let* ((s (read-char stream)))
        (cond
          ((and (not escape) (eq #\" s)) nil)
          ((and (not escape) (eq #\\ s)) (read-string t))
          (t (cons s (read-string nil)))
    )))
  )
  (list 'string (coerce (read-string nil) 'string))
))

(defun check-char (stream c)
  (let* ((s (peek-char nil stream nil)))
    (cond
      ((and (null c) (null s)) t)
      ((eq c s) (read-char stream) t)
      (t nil)
)))

(defun check-keyword (stream reference)
  (labels (
    (rec-check (ref acc)
      (let* ((s (read-char stream nil)))
        (cond
          ((null ref) (unread-char s stream) t)
          ((eq (first ref) s) (rec-check (rest ref) (cons s acc)))
          (t (notany (lambda (x) (unread-char x stream)) (cons s acc)) nil)
    )))
  )
  (rec-check (coerce reference 'list) '())
))

(defun read-next (stream)
  (cond

    ; Sanitize (EOF and spaces, tabs, etc.)
    ((check-char stream nil) (list (list 'the_end))) ; prevent an empty list while parsing
    ((some
       (lambda (x) (check-char stream x))
       (list #\space #\linefeed #\return #\tab #\newline)
     ) (read-next stream))

    ; Structures
    ((check-char stream #\{) (cons (list 'start_brace) (read-next stream)))
    ((check-char stream #\}) (cons (list 'end_brace) (read-next stream)))
    ((check-char stream #\[) (cons (list 'start_brack) (read-next stream)))
    ((check-char stream #\]) (cons (list 'end_brack) (read-next stream)))
    ((check-char stream #\,) (cons (list 'comma) (read-next stream)))
    ((check-char stream #\:) (cons (list 'colon) (read-next stream)))

    ; Values: string, numbers and bool
    ((check-char stream #\") (cons (get-string stream) (read-next stream)))
    ((is-digit stream) (cons (get-digit stream) (read-next stream)))
    ((check-keyword stream "true") (cons (list 'bool t) (read-next stream)))
    ((check-keyword stream "false") (cons (list 'bool nil) (read-next stream)))

    ; Errors
    (t (list 'error (peek-char nil stream nil))
)))

;(print (read-next *standard-input*))
