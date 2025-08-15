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
