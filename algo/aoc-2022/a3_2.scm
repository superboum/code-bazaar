; BITSET FOR ASCII CHARS
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

; -> s: string
; <- ascii-set
(define (string->ascii-set s)
  (let ([as (make-ascii-set)])
    (map (lambda (c) (ascii-set-add! as c)) (string->list s))
    as))

(define (string->filtered-ascii-set f s)
  (let ([as (make-ascii-set)])
    (map (lambda (c) (if (ascii-set-in? f c) (ascii-set-add! as c) '())) (string->list s))
    as))

; -> as: ascii-set, lc: list(char)
; <- char
(define (ascii-set-find as lc)
  (cond
    ((null? lc) #\nul)
    ((ascii-set-in? as (car lc)) (car lc))
    (#t (ascii-set-find as (cdr lc)))
))

(define (get-badge line1 line2 line3)
  (ascii-set-find
    (string->filtered-ascii-set (string->ascii-set line1) line2)
    (string->list line3)))

; -> c: char - an ascii char
; <- integer - the integer coding asked by the challenge
(define (letter-coding c)
  (cond
    ((char<=? #\A c #\Z) (- (char->integer c) (char->integer #\A) -27))
    ((char<=? #\a c #\z) (- (char->integer c) (char->integer #\a) -1))
    (#t 0)
))

(define (stdin) (get-line (current-input-port))) 

(define (result acc)
  (let ([line (stdin)])
    (cond
      ((eof-object? line) acc)
      (#t (result (+ acc (letter-coding (get-badge line (stdin) (stdin))))))
)))

(format #t "~a" (result 0))


