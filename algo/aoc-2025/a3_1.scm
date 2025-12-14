(define (bank port xtract-fn acc)
  (letrec ([loop (lambda ()
    (let ([v (get-char port)])
      (cond
	((and (char? v) (char-numeric? v)) 
	 (bank 
	   port 
	   xtract-fn 
	   (xtract-fn (- (char->integer v) (char->integer #\0)) acc)))
	(#t acc))))])
    (loop)))

(define (update-pair v acc)
  ;(format #t "l: ~a~%" acc)
  (let* ([last-pair (car acc)]
	 [l (car last-pair)]
	 [r (cadr last-pair)]
	 [acc2 (if (> v r) `((,l ,v)) acc)]
	 [acc3 (if (> v l) (cons `(,v -1) acc2) acc2)])
    acc3))

(define (best-pair acc)
  (let* ([cur (car acc)] [rest (cdr acc)] [l (car cur)] [r (cadr cur)])
    (cond
      ((and (>= l 0) (>= r 0)) (+ (* 10 l) r))
      (#t (best-pair rest)))))

(define (bank-jolt port)
  (best-pair (bank port update-pair '((-1 -1)))))


(define (battery port log-fn)
  (let ([v (peek-char port)])
    (cond
      ((and (char? v) (char-numeric? v)) 
       (cons (log-fn port) (battery port log-fn)))
      (#t '()))))

(format #t "Result: ~a~%" (apply + (battery (current-input-port) bank-jolt)))
