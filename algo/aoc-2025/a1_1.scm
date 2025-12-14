(define (apply-rot state side amount)
  (mod (+ state (if (eq? side #\R) amount (* amount -1))) 100))

(define (clic prev)
  (let*
    ([side (get-char (current-input-port))]
     [amount (get-datum (current-input-port))]
     [_ (get-line (current-input-port))])
    (cond
      ((or (eof-object? side) (eof-object? amount)) 0)
      ((or (eq? side "") (eq? amount "")) (clic prev))
      (#t (let* 
        ([next (apply-rot prev side amount)]
         [toadd (if (= next 0) 1 0)])
        (+ toadd (clic next))
)))))

(format #t "Code: ~a~%" (clic 50))
