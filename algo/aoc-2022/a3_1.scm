; BITSET FOR ASCII CHARS
; a 128 bits, ie. representing all the 128 possible characters of ASCII
(define (make-ascii-set) (make-bytevector 16 0))
(define (ascii-set-add! bv c)
  (let-values ([(idx shift) (div-and-mod (char->integer c) 8)])
    (bytevector-u8-set! 
      bv
      idx
      (logbit1 shift (bytevector-u8-ref bv idx))
)))
(define (ascii-set-in? bv c) 
  (let-values ([(idx shift) (div-and-mod (char->integer c) 8)])
    (logbit? shift (bytevector-u8-ref bv idx))))



(define (result)
  (let ([line (get-line (current-input-port))])
    (cond
      ((eof-object? line) #t)
      (#t
        (format #t "~a~%" line)
        (result)))))

;(result)
