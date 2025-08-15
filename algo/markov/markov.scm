(define PREV-TOKENS 3)

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
; WINDOWING

(define (apply-token-line-win prev-sz token-fx learn-fx)
  (lambda (line acc)
    (let* (
	[learn-acc (cdr (assoc 'learn acc))]
	[prev-tokens (cdr (assoc 'prev-tokens acc))]
	[line-tokens (token-fx line)]
	[win-tokens (append prev-tokens line-tokens)]
	[next-prev-tokens (list-tail win-tokens (max 0 (- (length win-tokens) prev-sz)))]
      )
      `(
	(learn . ,(learn-fx learn-acc prev-sz win-tokens)) 
	(prev-tokens . ,next-prev-tokens)
      )
)))

(define (apply-line ip acc fx)
  (let* ([maybe-line (get-line ip)])
    (cond
      ((eof-object? maybe-line) acc)
      (#t 
       (apply-line ip (fx maybe-line acc) fx))
)))

; ---
; MARKOV LEARN

; Used to register token association (from a ref text)
; Can register an arbitrary serie of token
; Prev must be reversed, such that when we will infer, we will be able
; to give more importance to the first previous word, then find if the previous one matches, etc.
(define (learn-markov-tuple subchain rev-prevs next)
  (hashtable-update! 
    subchain 
    (car rev-prevs)
    (cond
      ((null? (cdr rev-prevs)) (lambda (tx) (hashtable-update! tx next add1 0) tx))
      (#t (lambda (tx) (learn-markov-tuple tx (cdr rev-prevs) next))))
    (make-hashtable string-hash string=?))
  subchain
)

(define (learn-markov-sequence chain prev-sz seq)
  (cond
    ((< (length seq) (+ 1 prev-sz)) chain)
    (#t (let ([prevs (firsts seq prev-sz)] [cur (list-ref seq prev-sz)])
	(learn-markov-sequence 
	  (learn-markov-tuple chain (reverse prevs) cur)
	  prev-sz
	  (cdr seq))))
))

; ---
; MARKOV TEACH

(define (markov-init subchain)
  (let* ([subkeys (hashtable-keys subchain)]
	 [sel-key (vector-ref subkeys (random (vector-length subkeys)))]
	 [new-sub (hashtable-ref subchain sel-key #f)])
    (cond
      ((hashtable? new-sub) (cons sel-key (markov-init new-sub)))
      (#t `())
)))

(define (markov-next-token subchain rev-prevs)
  (cond
    ; in this case, we found the "next" leaf as we crossed all "prev" ones
    ((null? rev-prevs) (let ([nexts (hashtable-keys subchain)])
      (vector-ref nexts (random (vector-length nexts)))))
    ; otherwise, we need to progress in the tree, if the word exists
    ((hashtable-contains? subchain (car rev-prevs))
      (markov-next-token (hashtable-ref subchain (car rev-prevs) #f) (cdr rev-prevs)))
    ; if the ref does not exist, peek a random prev
    (#t (let* ([all-prevs (hashtable-keys subchain)] 
	       [replaced (vector-ref all-prevs (random (vector-length all-prevs)))])
      (markov-next-token (hashtable-ref subchain replaced #f) (cdr rev-prevs))))
))

(define (markov-until chain initial-text prev-sz stop?)
  (cond
    ((stop? initial-text) initial-text)
    (#t (markov-until 
	  chain 
	  (cons 
	    (markov-next-token 
	      chain 
	      (firsts initial-text prev-sz)) 
	    initial-text) 
	  prev-sz
	  stop?))
))

;----
(define (add-spaces lst)
  (cond
    ((null? lst) '())
    (#t (cons (car lst) (cons " " (add-spaces (cdr lst)))))))

(define (markov->string markov-tokens)
  (apply string-append (add-spaces (reverse markov-tokens))))

;---
; STOP CONDITIONS
;
(define (end-of-sentence txt)
  (or
    (string=? (car txt) ".")
    (string=? (car txt) "!")
    (string=? (car txt) "?")))

(define (fixed-tokens txt)
  (> (length txt) 3000))

;----
; I/O
; Open file
(define (learn-capital)
  (call-with-port 
    (open-file-input-port 
      "capital.txt" 
      (file-options) 
      (buffer-mode line) 
      (native-transcoder))

    (lambda (source-fd)
      (cdar (apply-line 
        source-fd 
        `((learn . ,(make-hashtable string-hash string=?)) (prev-tokens . ())) 
        (apply-token-line-win PREV-TOKENS extract-token learn-markov-sequence))))
))

(define marx (learn-capital))
(define (demo1) (markov->string (markov-until marx (markov-init marx) PREV-TOKENS fixed-tokens)))
