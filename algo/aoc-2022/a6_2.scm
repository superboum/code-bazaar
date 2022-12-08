;-- utils
(define (drop l n)
  (cond
    ((null? l) '())
    ((eq? n 0) l)
    (#t (drop (cdr l) (- n 1)))
))

;-- bitset ascii
; a 128 bits, ie. representing all the 128 possible characters of ASCII
(define (make-ascii-set) (make-bytevector 16 0))
(define (ascii-set-add! bv c)
  (let-values ([(idx shift) (div-and-mod (char->integer c) 8)])
    (bytevector-u8-set!
      bv
      idx
      (logbit1 shift (bytevector-u8-ref bv idx)))
    bv
))
(define (ascii-set-in? bv c)
  (let-values ([(idx shift) (div-and-mod (char->integer c) 8)])
    (logbit? shift (bytevector-u8-ref bv idx))))
(define (ascii-set-clear! bv) (bytevector-fill! bv 0) bv)

;-- window implementation
(define (win-shift port win) (append (drop win 1) (list (get-char port))))

(define (win-init port s)
  (cond
    ((eq? s 0) '())
    (#t (cons (get-char port) (win-init port (- s 1))))
))

(define (win-check l as)
  (cond
    ((null? l) #t)
    ((ascii-set-in? as (car l)) #f)
    (#t (win-check (cdr l) (ascii-set-add! as (car l))))
))


(define (win-next port win curs)
  (cond
    ((win-check win (make-ascii-set)) curs)
    (#t (win-next port (win-shift port win) (+ curs 1)))
))

(define (parse-line port)
  (let* ([win-size 14]
         [marker (win-next port (win-init port win-size) win-size)])
    (get-line port)
    marker
))

(define (parse port)
  (cond
    ((eof-object? (lookahead-char port)) '())
    (#t (cons (parse-line port) (parse port)))
))

(format #t "~a~%" (parse (current-input-port)))
