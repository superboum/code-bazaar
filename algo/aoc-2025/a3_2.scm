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

(define (na _) -1)
(define (replacement v num-before cur num-after)
  (cond
    ((= cur -1) (list (append num-before (list v) (map na num-after))))
    ((> v cur) 
      (append 
        (list (append num-before (list v) (map na num-after)))
        (if (null? num-after) '() (replacement v (append num-before (list cur)) (car num-after) (cdr num-after)))))
    ((null? num-after) (list (append num-before (list cur))))
    (#t (replacement v (append num-before (list cur)) (car num-after) (cdr num-after)))))

(define (all fn l) (cond ((null? l) #t) ((fn (car l)) (all fn (cdr l))) (#t #f)))
(define (some fn l) (cond ((null? l) #f) ((fn (car l)) #t) (#t (some fn (cdr l)))))
(define (update-pair v gacc)
  ;(format #t "acc: ~a~%~%" gacc)
  (letrec (
    [loop (lambda (acc)
	(cond
	  ((null? acc) '())
	  (#t (let* (
	    [cur (car acc)] 
	    [rest (cdr acc)] 
	    [cd (replacement v '() (car cur) (cdr cur))]
	    [cd-enough (some (lambda (c) (all (lambda (v) (> v -1)) c)) cd)]
	  )
	    (if cd-enough cd (append cd (loop rest)))))))])
    (loop gacc)))

(define (build-num lnum)
  (letrec ([loop (lambda (v pow)
    (cond
      ((null? v) 0)
      (#t (+ (* (car v) (expt 10 pow)) (loop (cdr v) (- pow 1))))))])
    (loop lnum (- (length lnum) 1))))

(define (best-pair acc)
  (cond
    ((all (lambda (x) (> x -1)) (car acc)) (build-num (car acc)))
    (#t (best-pair (cdr acc)))
))

(define (bank-jolt port)
  (best-pair (bank port update-pair '((-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)))))


(define (battery port log-fn)
  (let ([v (peek-char port)])
    (cond
      ((and (char? v) (char-numeric? v)) 
       (cons (log-fn port) (battery port log-fn)))
      (#t '()))))

(format #t "Result: ~a~%" (apply + (battery (current-input-port) bank-jolt)))
