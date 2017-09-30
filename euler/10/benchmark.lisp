(defconstant limit 200000)
(defconstant sievebound (floor (- limit 1) 2))
(defconstant crosslimit (floor (- (isqrt limit) 1) 2))

(print "The limit does not correspond to the project euler's problem !")

; Solve without a sieve
(defun can-be-divided (n l stop)
  (cond
    ((> stop n) nil)
    ((eq l nil) nil)
    ((eq (mod n (car l)) 0) (car l))
    (t (can-be-divided n (cdr l) stop))
  )
)

(defun compute-primes (iterator limit2 primes)
  (cond
    ((>= iterator limit2) primes)
    ((can-be-divided iterator primes (isqrt iterator)) (compute-primes (+ 2 iterator) limit2 primes))
    (t (compute-primes (+ 2 iterator) limit2 (append primes (list iterator))))
))

; Solve with a list
(defun solve-with-list ()
  (let (
      (sieve (make-list sievebound))
      (sum 2))

    ; Create sieve
    (loop
      for i from 1 to crosslimit
      when (not (nth (- i 1) sieve))
        do (loop for j from (* 2 i (+ i 1)) to sievebound by (+ (* 2 i) 1)
          do (setf (nth (- j 1) sieve) 1)
    ))

    ; Compute sum from sieve
    (loop
      for i from 1 to sievebound
      when (not (nth (- i 1) sieve)) do (setq sum (+ sum (* 2 i) 1))
    )

    sum
))

; Solve with an array
(defun solve-with-array ()
  (let (
      (sieve (make-array sievebound :element-type 'bit :initial-element 0))
      (sum 2))

    ; Create sieve
    (loop
      for i from 1 to crosslimit
      when (zerop (sbit sieve (- i 1)))
        do (loop for j from (* 2 i (+ i 1)) to sievebound by (+ (* 2 i) 1)
          do (setf (sbit sieve (- j 1)) 1)
    ))

    ; Compute sum from sieve
    (loop
      for i from 1 to sievebound
      when (zerop (sbit sieve (- i 1) )) do (setq sum (+ sum (* 2 i) 1))
    )

    sum
))

; From rosetta code
; https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Common_Lisp
(defun sieve-odds (maximum) "sieve for odd numbers"
  (cons 2
        (let ((maxi (ash (1- maximum) -1)) (stop (ash (isqrt maximum) -1)))
          (let ((sieve (make-array (1+ maxi) :element-type 'bit :initial-element 0)))
            (loop for i from 1 to maxi
              when (zerop (sbit sieve i))
              collect (1+ (ash i 1))
              and when (<= i stop) do
                (loop for j from (ash (* i (1+ i)) 1) to maxi by (1+ (ash i 1))
                   do (setf (sbit sieve j) 1)))))))

(print "Solve without a sieve")
(time (print (reduce #'+ (compute-primes 3 limit '(2)))))

(print "Solve with a list")
(time (print (solve-with-list)))

(print "Solve with an array")
(time (print (solve-with-array)))

(print "Solve with the sieve from Rosetta Code (array)")
(time (print (reduce #'+ (sieve-odds limit))))
