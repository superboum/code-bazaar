# LispW

A lisp as cringe as your favorite 2010 lip dub clip (did you get the joke?).

Example of computing 5! with recursion:

```lisp
(letrec (fact (lambda (x) 
  (if 
    (eq x 1) 1 
    (* x (fact (- x 1)))))) 
  (fact 5))
```

## Quickstart

```bash
make bin/repl
./bin/repl
```

## Features

### Lang props

Intended to be a mix between Scheme & Clojure.

 - [x] lexical scoping 
 - [ ] recursivity
   - [x] basic recursivity
   - [ ] corecursivity (streams, generator, etc.)
   - [ ] mutual recursion (a calls b, b calls a)
 - [x] immutability
 - [x] few syntactic sugar
 - [x] *should be* mostly normal order 

### Memory Management

- [x] Reference Counting
- [x] Slab allocator
- [x] Detect & crash on memory leaks

### Datatype

Our language has a single datastructure: atoms.
It is designed to fit on 24 bytes.
 - [X] public atoms
   - [x] nil (nil means `false`, everything else `true`)
   - [x] symbols
   - [X] int64
   - [x] strings
   - [ ] short strings (encoded on 16 bytes; 15 characters)
   - [x] pairs
   - [ ] floats
 - [x] runtime atoms
   - [x] closure (lambda + env)
   - [x] weak pointers
   - [x] fx1, fx2, fx3 for C function bindings

### S-expr serialization / deserialization

 - [x] base s-expr lexer
 - [x] base s-expr parser
 - [x] base s-expr serializer
 - [x] support comments
 - [ ] support ticks syntaxic sugar (quote `'`, quasiquote `\``, unquote `,`)
 - [ ] handle dot syntax `(a . b)`; ie. the case where `cdr(a)` is not NIL or a PAIR

### Special forms

 - [X] let
 - [x] lambda
 - [x] quote
 - [x] if
 - [x] thunk (delayed execution / normal order)
 - [x] define
 - [x] macro
 - [ ] quasiquote + unquote
 - [ ] call/cc (call with continuation)
   - [ ] implement exceptions

### Standard Library

 - [x] C functions
   - [x] boolean logic (`if`)
   - [x] arithmetic (`+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`)
   - [x] list processing
     - [x] `reverse`
     - [x] basic utilities like `car`, `cdr`, `cons`, etc.
   - [ ] string processing
 - [x] Lisp functions / macros
   - [ ] recursivity
     - [x] Y combinator
     - [x] letrec
     - [ ] letrec* for mutual recursivity
   - [x] and / or
   - [ ] cond
   - [ ] map / fold
   - [ ] streams (lazy lists) / corecursivity
 - [ ] Expose interpreter internals
   - [x] `sexpr` / `parse`
   - [ ] eval


### I/O

*Not yet implemented/designed. I'm thinking to something built around linux [io_uring](https://man7.org/linux/man-pages/man7/io_uring.7.html).*

### Optimizations

 - [x] static symbols
 - [x] memoized thunks
 - [ ] avoid, when possible, rc allocs/deallocs in eval/apply
 - [ ] Lexical Addressing (or Static Scope Resolution)
 - [ ] Tail Call Optimization

### Tree-Walk Interpreter

 - [x] basic REPL (read stdin until new line, eval, output to stdout)
 - [x] proper REPL (read stdin a - multiline - lisp expression, eval & mutate env through define, output to stdout)
 - [ ] evolve flat env to a ~frame thingy
 - [ ] recover on errors (requires call/cc)
 - [ ] display a backtrace on errors (requires to rewrite with a proper frame abstraction)
 - [ ] break loop (on error, provide a repl in the context of the exception)

### Bytecode Interpreter

 - [ ] Bytecode definition
 - [ ] Bytecode emission
 - [ ] VM

*Note: In the long run I would like to optimize the reference counting
logic by integrating it to the VM bytecode. BUT maybe not for a first pass.*

## Other limitations...

...that come to my mind

 - No error management (program just crash)
 - No static type
 - No proper check of compound argument number leading to weird bug.
   - eg. `((lambda (a b) (+ a b)) 3)` (missing `b` binding) leads to a weird error.

## Some thinking

First, I think to a stack-based VM/bytecode.
Here are some ideas.

### Research/examples

Lisp expression:

```lisp
(+ 2 3)
```

Possible bytecode:

```bytecode
--- DATA ---
label plus: 
 +

--- CODE ---
label main:
  PUSH_NIL
  PUSH_INT 2
  PUSH_INT 3
  PUSH_SYMBOL &label_plus
  FETCH_ENV
  APPLY
```

---

Lisp expression:

```lisp
((lambda (x) (+ x x)) 1)
```

Possible bytecode:

```bytecode
--- DATA ---
label plus:
 +
label x:
 x

-- CODE --
label anon1:
  PUSH_NIL
  PUSH_SYMB &label_x
  RESOLVE_ENV
  PUSH_SYMB &label_x
  RESOLVE_ENV
  PUSH_SYMB &label_plus
  RESOLVE_ENV
  CALL   // pop +, x, x ; push res
  RETURN // pop res, pop ip, goto ip, push res
label main:
  PUSH_NIL
  PUSH_INT 1
  CLOSURE &anon1
  CALL
  // stack should be: [2]
```


## Resources

What motivated me to start this project: [ William Byrd on "The Most Beautiful Program Ever Written" [PWL NYC] ](https://www.youtube.com/watch?v=OyfBQmvr2Hc)

I've read (at least partially):
 - Crafting Interpreters by Robert Nystrom
 - SICP by H. Abelson, G. J. Sussman and J. Sussman

On my reading list:
 - Lisp In Small Pieces by C. Queinnec
 - Paradigms of Artificial Intelligence Programming (PAIP) by P. Norvig
