;;; Day 3: Lobby - Battery Joltage Part 2
;;; R5RS Scheme Solution

(define (char-digit? c)
  (and (char>=? c #\0) (char<=? c #\9)))

(define (char->digit c)
  (- (char->integer c) (char->integer #\0)))

;;; Converts a string into a list of integers, ignoring non-digits
(define (string->digit-list s)
  (let loop ((i 0) (acc '()))
    (if (= i (string-length s))
        (reverse acc)
        (let ((c (string-ref s i)))
          (if (char-digit? c)
              (loop (+ i 1) (cons (char->digit c) acc))
              (loop (+ i 1) acc))))))

;;; Converts a list of digits (e.g., '(1 2 3)) into a number (123)
(define (digits->number digit-lst)
  (let loop ((lst digit-lst) (acc 0))
    (if (null? lst)
        acc
        (loop (cdr lst) (+ (* acc 10) (car lst))))))

;;; Finds the first occurrence of the maximum digit within the first `limit` items.
;;; Returns a pair: (max-digit . remaining-list-after-max)
(define (find-best-in-window lst limit)
  (let loop ((curr-lst lst)
             (index 0)
             (best-val -1)
             (best-index -1))
    (if (or (null? curr-lst) (= index limit))
        ;; Reconstruction step: 
        ;; We found the best index. We need to traverse original 'lst' 
        ;; to get the tail starting strictly after best-index.
        (let rec-split ((l lst) (i 0))
          (if (= i best-index)
              (cons (car l) (cdr l)) ;; (found-digit . rest)
              (rec-split (cdr l) (+ i 1))))
        ;; Search step
        (let ((val (car curr-lst)))
          (if (> val best-val)
              (loop (cdr curr-lst) (+ index 1) val index)
              (loop (cdr curr-lst) (+ index 1) best-val best-index))))))

;;; Selects exactly k digits from digits-lst to maximize the number formed.
(define (maximize-subsequence digits-lst k)
  (let loop ((lst digits-lst) 
             (needed k) 
             (acc '()))
    (if (= needed 0)
        (reverse acc)
        (let* ((n (length lst))
               ;; We must pick 1, leaving (needed - 1) for later.
               ;; So we can only search the first (n - (needed - 1)) elements.
               (window-size (+ (- n needed) 1))
               (result (find-best-in-window lst window-size))
               (picked-digit (car result))
               (remaining-lst (cdr result)))
          (loop remaining-lst (- needed 1) (cons picked-digit acc))))))

;;; Wraps logic for a single line
(define (solve-bank-part2 digits)
  (if (< (length digits) 12)
      0 ;; Handle edge case where line is too short (though input implies valid)
      (let ((chosen-digits (maximize-subsequence digits 12)))
        (digits->number chosen-digits))))

;;; Helper to read a full line from stdin
(define (read-line)
  (let loop ((chars '()))
    (let ((c (read-char)))
      (cond 
        ((eof-object? c) 
         (if (null? chars) c (list->string (reverse chars))))
        ((char=? c #\newline) 
         (list->string (reverse chars)))
        (else 
         (loop (cons c chars)))))))

;;; Main execution loop
(define (main)
  (let loop ((total-output 0))
    (let ((line (read-line)))
      (if (eof-object? line)
          (begin
            (display "Total Output Joltage (Part 2): ")
            (display total-output)
            (newline))
          (let ((digits (string->digit-list line)))
            (if (null? digits)
                (loop total-output)
                (loop (+ total-output (solve-bank-part2 digits)))))))))

(main)
