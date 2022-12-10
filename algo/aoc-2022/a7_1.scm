; -- utils
(define (list-contains-string? l s)
  (cond
    ((null? l) #f)
    ((string=? (car l) s) #t)
    (#t (list-contains-string? (cdr l) s))
))

; --- lexer
; syntax: $
; keywords: cd, ls, dir
; types: word, int

(define (lex-word port)
  (let ([c (get-char port)])
    (cond
      ((or (eof-object? c) (eq? c #\space) (eq? c #\newline)) '())
      (#t (cons c (lex-word port)))
)))

(define kw '("cd" "ls" "dir"))
(define (lexer port)
  (let* ([cw (lex-word port)] [w (list->string cw)])
    (cond
      ((null? cw) 'eof)
      ((string=? w "$") 'prompt)
      ((list-contains-string? kw w) (string->symbol w))
      ((and (char>=? (car cw) #\0) (char<=? (car cw) #\9)) `(int . ,(string->number w)))
      (#t  `(word . ,w))
)))
(define (lexer-peekable port)
  (let ([tmp (lexer port)])
    (lambda (opt)
      (cond
        ((eq? opt 'peek) tmp)
        (#t (let ([res tmp]) (set! tmp (lexer port)) res))
))))

; --- parser
; input: cmdBlock, input | <eof>
; cmdBlock: <$>, <cd>, path | <$>, <ls>, infoLine
; path: <word>
; infoLine: (dirLine | fileLine), infoLine | <none>
; dirLine: <dir>, <word>
; fileLine: <int>, <word>

(define (parser tokens obs)
  (let ([t (tokens 'peek)])
    (cond
      ((eq? 'eof t) obs)
      (#t (parser tokens (parse-term tokens obs))) 
)))

(define (parse-term tokens obs)
  (let ([t (tokens 'peek)])
    (cond
      ((eq? 'prompt t) (parse-block tokens obs))
      (#t (raise "invalide token for parse-term"))
)))

(define (parse-block tokens obs)
  (assert (eq? (tokens 'read) 'prompt))
  (let ([t (tokens 'read)])
    (cond
      ((eq? t 'cd) (obs `(cd . ,(cdr (tokens 'read))))) ; we should type-check the path...
      ((eq? t 'ls) (obs `(ls . ,(parse-ls-res tokens))))
      (#t (raise "invalid token for parse-cmd"))
)))

(define (parse-ls-res tokens)
  (let ([t (tokens 'peek)])
    (cond
      ((or (eq? t 'prompt) (eq? t 'eof)) '())
      (#t (cons (parse-info tokens) (parse-ls-res tokens)))
)))

(define (parse-info tokens)
  (let ([t (tokens 'read)])
    (cond 
      ((eq? 'dir t) `(dir ,(cdr (tokens 'read))))
      ((eq? 'int (car t)) `(file ,(cdr t) ,(cdr (tokens 'read))))
      (#t (raise "invalid token for parse-info"))
)))

; --- build filesystem tree
(define (obs-gen fstree workdir)
  (lambda (v)
    (cond
      ((eq? (car v) 'cd) (obs-gen fstree (cwd workdir (cdr v))))
      ((eq? (car v) 'ls) (obs-gen (updtree fstree (cdr (reverse workdir)) (cdr v)) workdir))
      ;(#t (list fstree workdir))
      (#t fstree)
)))

(define (cwd path rel)
  (cond
    ((string=? ".." rel) (cdr path))
    (#t (cons rel path))))

(define (updtree fstree path files)
  (cond
    ((null? path) (append fstree (fstreefmt files)))
    (#t 
     (let-values ([(leaf others) (leaf-extract (cddr fstree) (car path))])
       (append 
         (list (car fstree) (cadr fstree) (updtree leaf (cdr path) files))
         others)
))))

(define (fstreefmt files)
  (map (lambda (f) (if (eq? (car f) 'dir) `(,(cadr f) -1) (reverse (cdr f)))) files))

(define (leaf-extract leaves target)
  (letrec ([inner (lambda (leaves target acc)
    (cond
      ((null? leaves) (raise "not found"))
      ((string=? (caar leaves) target) (values (car leaves) (append acc (cdr leaves))))
      (#t (inner (cdr leaves) target (cons (car leaves) acc)))))])
    (inner leaves target '())))

; -- compute size
(define (fs-folder-size fstree)
  (cond
    ((> (cadr fstree) 0) fstree)
    (#t 
     (let* ([new-leaves (map fs-folder-size  (cddr fstree))]
            [size (apply + (map (lambda (f) (cadr f)) new-leaves))])
        (cons (car fstree) (cons size new-leaves))
))))

; -- list folders with size smaller than x
(define (folder-smaller-than fstree x)
  (cond
    ((null? (cddr fstree)) '()) ; this is a file
    ((> (cadr fstree) x) (apply append (map (lambda (f) (folder-smaller-than f x)) (cddr fstree))))
    (#t (cons (cadr fstree) (apply append (map (lambda (f) (folder-smaller-than f x)) (cddr fstree)))))
))

; --- glue
(define lex-stdin (lexer-peekable (current-input-port)))
(let* ([visitor (parser lex-stdin (obs-gen '("/" 0) '()))]
       [fstree (visitor '(dump))]
       [stats (fs-folder-size fstree)]
       [small-folder (folder-smaller-than stats 100000)])
  (format #t "~a~%" (apply + small-folder)))
