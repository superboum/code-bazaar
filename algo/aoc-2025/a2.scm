(define (get-num port)
  (letrec ([loop (lambda ()
    (let ([v (get-char port)])
      (cond
        ((and (char? v) (char-numeric? v)) (cons v (loop)))
        (#t '())
    )))])
    (string->number (list->string (loop)))
))

(define (capture-ranges aggr-result-fn process-range-fn)
  (let*
    ([beg (get-num (current-input-port))]
     [end (get-num (current-input-port))])
    (cond
      ((or (not beg) (not end)) 0)
      (#t (aggr-result-fn 
	    (process-range-fn beg end) 
	    (capture-ranges aggr-result-fn process-range-fn)
      ))
)))

(define (is-invalid-id id repeat-pattern-max) 
  (letrec* 
    ([starting-pow (flonum->fixnum (/ (log id) (log 10)))]
     [is-valid-pow? (>= starting-pow 1)]
     [loop-pow (lambda (v-pow)
       (letrec*
        ([suffix (mod id (expt 10 v-pow))]
         [is-valid-suffix? (and (> suffix 0) (>= suffix (expt 10 (- v-pow 1))))]
         [loop-sfx (lambda (prev limit)
	   ;(format #t "prev: ~a // suffix: ~a~%" prev suffix)
           (cond
	     ; found
	     ((= prev id) #t)
	     ; stop condition
	     ((or (> prev id) (<= limit 0)) #f)
	     (#t (loop-sfx (+ (* prev (expt 10 v-pow)) suffix) (- limit 1)))
          ))]
          [partial-invalid? (and is-valid-suffix? (loop-sfx suffix repeat-pattern-max))])
        (cond
	  (partial-invalid? #t)
	  ((<= v-pow 1) #f)
	  (#t (loop-pow (- v-pow 1))))))])
    (and is-valid-pow? (loop-pow starting-pow))
))

(define (sum-invalid-ids beg end repeat-pattern-max)
  (cond
    ((> beg end) 0)
    ((is-invalid-id beg repeat-pattern-max) 
     (+ beg (sum-invalid-ids (+ 1 beg) end repeat-pattern-max)))
    (#t 
     (sum-invalid-ids (+ 1 beg) end repeat-pattern-max))
))

(define PART1 1)
(define PART2 1000)

(format 
  #t 
  "Result: ~a~%"
  (capture-ranges + (lambda (beg end) (sum-invalid-ids beg end PART2))))
