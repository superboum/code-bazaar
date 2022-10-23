#!/usr/bin/sbcl --script

(write (reduce #'+ (map 'list #'digit-char-p (coerce (write-to-string (expt 2 1000)) 'list))))
