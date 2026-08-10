; Just a short test to make sure we are lexically scoped
; and not dynamically scoped. In this example:
; - lexically scoped should return 2
; - dynamically scoped should return 1
(let* ([x 2] [get-x (lambda () x)] [x 1])
  (get-x))
