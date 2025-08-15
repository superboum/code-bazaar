(define (find-eot-idx line-part bidx eidx specialization)
  (cond
    ((eq? specialization 'punct) (values specialization bidx eidx))
    ((= eidx (string-length line-part)) (values specialization bidx eidx))
    (#t (let ([curchar (string-ref line-part eidx)])
	  (cond
	    ((and (eq? specialization 'none) (char-whitespace? curchar))
	     (find-eot-idx line-part (+ bidx 1) (+ eidx 1) 'none))
	    ((and 
	       (or (eq? specialization 'none) (eq? specialization 'word))
	       (or (char-alphabetic? curchar) (char-numeric? curchar)))
	     (find-eot-idx line-part bidx (+ eidx 1) 'word))
	    ((eq? specialization 'none) 
	     (values 'punct bidx (+ eidx 1)))
	    (#t (values specialization bidx eidx))
     )))
))

; First naive token implementation
(define (extract-token line-part)
  (cond
    ((= (string-length line-part) 0) '())
    (#t (let*-values 
	  ([(spec bidx eidx) (find-eot-idx line-part 0 0 'none)])
	  (cons
	    (substring line-part bidx eidx)
	    (extract-token (substring line-part eidx (string-length line-part)))
    )))
))


(define (line-counter line acc) (+ acc 1))

; unroll
(define (apply-line ip fx acc)
  (let ([maybe-line (get-line ip)])
    (cond
      ((eof-object? maybe-line) acc)
      (#t (apply-line ip fx (fx maybe-line acc)))
)))

; Open file
(call-with-port 
  (open-file-input-port 
    "capital.txt" 
    (file-options) 
    (buffer-mode line) 
    (native-transcoder))

  (lambda (source-fd)
    (format #t "~a~%" (apply-line source-fd line-counter 0)))

)
