(let*
  ([ascii/lparen 40]
   [ascii/rparen 41]
   [filepath "examples/aoc_2015_day_1.txt"]
   ; The full example of AOC 2015 day 1 triggers a stackoverflow
   ; because we have no tail call optimization.
   ;[filepath "examples/aoc_2015_day_1.txt.full"]
  )
  (reduce 
    (lambda (acc c)
      (cond
        [(eq c ascii/lparen) (+ acc 1)]
        [(eq c ascii/rparen) (- acc 1)]
        [t acc]))
     0 
     (io/file filepath)))
