#!/usr/bin/sbcl --script

(defun chain-length (n)
  (cond
    ((eq n 1) 1)
    ((eq (mod n 2) 0) (+ 1 (chain-length (/ n 2))))
    (t (+ 1 (chain-length (+ (* 3 n) 1))))
))

(defun longest-chain (n starting best)
  (cond
    ((eq n 0) starting)
    ((> (chain-length n) best) (longest-chain (- n 1) n (chain-length n)))
    (t (longest-chain (- n 1) starting best))
))

(write (longest-chain 1000000 0 0))
