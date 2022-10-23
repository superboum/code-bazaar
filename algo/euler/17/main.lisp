#!/usr/bin/sbcl --script

(defconstant
  lunits
  (list
    ""
    "one"
    "two"
    "three"
    "four"
    "five"
    "six"
    "seven"
    "eight"
    "nine"
))

(defconstant
  first-ltens
  (list
    "ten"
    "eleven"
    "twelve"
    "thirteen"
    "fourteen"
    "fifteen"
    "sixteen"
    "seventeen"
    "eighteen"
    "nineteen"
))

(defconstant
  ltens
  (list
    "twenty"
    "thirty"
    "forty"
    "fifty"
    "sixty"
    "seventy"
    "eighty"
    "ninety"
))

(defun create-ten (ten units) (map 'list (lambda (x) (concatenate 'string ten x)) units))
(defun create-tens (tens units) (reduce #'append (map 'list (lambda (x) (create-ten x units)) tens)))
(defun to-hundred () (append lunits first-ltens (create-tens ltens lunits)))

(defun create-hundreds() (cons "" (map 'list (lambda (x) (concatenate 'string x "hundredand")) (rest lunits))))
(defun to-thousand() (
  reduce #'append (
    map 'list (lambda (an-hundred) (
      map 'list (lambda (x) (concatenate 'string an-hundred x)) (to-hundred))
      )
    (create-hundreds)
  )
))

(write (-
  (reduce #'+ (map 'list #'length (append (to-thousand) (list "onethousand"))))
  (* (length "and") 9) ; as, for "100", we write "one hundred" and not "one hundred and". Idem for 200, 300, 400...
))
