(define (apply-rot state side amount)
  (div-and-mod (+ state (if (eq? side #\R) amount (* amount -1))) 100))


(define (clic prev)
  (let*
    ([side (get-char (current-input-port))]
     [amount (get-datum (current-input-port))]
     [_ (get-line (current-input-port))])
    (cond
      ((or (eof-object? side) (eof-object? amount)) 0)
      ((or (eq? side "") (eq? amount "")) (clic prev))
      (#t (let*-values
        ([(pre-crossed1 next) (apply-rot prev side amount)]
	 [(pre-crossed2) (abs pre-crossed1)]
	 [(crossed) (cond 
	   ; fix the 0 specific case when rotating left.
           ((and (= 0 prev) (eq? side #\L)) (- pre-crossed2 1))
	   ((and (= 0 next) (eq? side #\L)) (+ pre-crossed2 1))
	   (#t pre-crossed2))
	 ])
	(format #t "~a~a ; ~a -> ~a ; ~a ~%" side amount prev next crossed)
        (+ crossed (clic next))
)))))

(format #t "Code: ~a~%" (clic 50))
