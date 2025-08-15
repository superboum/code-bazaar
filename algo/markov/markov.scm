; UTILS
(define (firsts lst n)
  (cond
    ((or (= n 0) (null? lst)) '())
    (#t (cons (car lst) (firsts (cdr lst) (- n 1))))
))

;---
; TOKEN EXTRACT

; Find Eof-Of-Token index, very basic logic
; 3 possibles tokens kind: 'none, 'punct and 'word
;
; Example output:
; > (find-eot-idx "  coucou, le monde!" 0 0 'none)
; 'word
; 2
; 8
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

; ---
; TEST FX
(define (line-counter any acc) (+ acc 1))

; ---
; MARKOV CHAIN

; Used to register token association (from a ref text)
(define (learn-markov-tuple chain prev next)
  (hashtable-update! 
    chain 
    prev 
    (lambda (tx) hashtable-update! tx next add1 0) 
    (make-eqv-hashtable)))

(define (learn-markov-sequence chain prev-sz seq)
  (cond
    ((< (length seq) (+ 1 prev-sz)) chain)
    (#t (let ([prevs (firsts seq prev-sz)] [cur (list-ref seq (+ 1 prev-sz))])
	(learn-markov-tuple chain (apply string-append prevs) cur)))
))

; ---
; WINDOWING

;(define (apply-token-line-win win-sz token-fx learn-fx)
;  (lambda (line acc)
;    (let*-values (
;        [(ext-acc win) acc] 
;        [(tokens) (token-fx line)]
;      )
;    )
;))

(define (apply-line ip acc fx)
  (let* ([maybe-line (get-line ip)])
    (cond
      ((eof-object? maybe-line) acc)
      (#t 
       (apply-line ip (fx maybe-line acc) fx))
)))

; (apply-line 
;    source-fd 
;    (make-token-line-win (make-markov))
;    (apply-token-line-win 3 extract-token)) 
;

;----

; I/O
; Open file
(call-with-port 
  (open-file-input-port 
    "capital.txt" 
    (file-options) 
    (buffer-mode line) 
    (native-transcoder))

  (lambda (source-fd)
    (format #t "~a~%" (apply-line source-fd 0 line-counter)))
)
