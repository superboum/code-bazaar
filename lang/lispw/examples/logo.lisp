; Inspired by the Logo language.
;
; A turtle can receive instructions:
; - either rotate right (RIGHT)
; - or rotate left (LEFT)
; - or move forward (MOVE)
;
; Example with a turtle starting with (x=0, y=0, orientation=EAST)
; > . . . .
; . . . . .
; . . . . .
;
; Here is a series of orders:
; MOVE MOVE RIGHT MOVE LEFT MOVE RIGHT MOVE RIGHT MOVE MOVE
;
; And the final position (with the path taken) once the order are applied:
; * * * . .
; . . * * .
; . < * * .
;
; The final position is (x=1, y=2, orientation=west)
(let*
  ([ori-cwise '(NORTH EAST SOUTH WEST NORTH)]
   [ori-ccwise (reverse ori-cwise)]
   [ori-next (letrec [do (lambda (ori ord) 
     (if (eq (car ord) ori) (cadr ord) (do ori (cdr ord))))] do)]
   [pos-x-next (lambda (x ori)
    (cond
      [(eq ori 'EAST) (+ x 1)]
      [(eq ori 'WEST) (- x 1)]
      [t x]))]
  [pos-y-next (lambda (y ori)
    (cond
      [(eq ori 'NORTH) (- y 1)]
      [(eq ori 'SOUTH) (+ y 1)]
      [t y]))]
  [logo
    (letrec 
    [do (lambda (pos-x pos-y ori instr)
      (cond
        [(null? instr) 
	 (list pos-x pos-y ori)]
        [(eq (car instr) 'LEFT) 
	 (do pos-x pos-y (ori-next ori ori-ccwise) (cdr instr))]
        [(eq (car instr) 'RIGHT) 
	 (do pos-x pos-y (ori-next ori ori-cwise) (cdr instr))]
        [(eq (car instr) 'MOVE) 
	 (do (pos-x-next pos-x ori) (pos-y-next pos-y ori) ori (cdr instr))]))]
    do)])
  (logo 0 0 'EAST 
	'(MOVE MOVE RIGHT MOVE LEFT MOVE RIGHT MOVE RIGHT MOVE MOVE)))


