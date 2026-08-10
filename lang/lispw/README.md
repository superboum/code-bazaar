# LispW

A lisp as cringe as your favorite 2010 lip dub clip (did you get the joke?).

Example of computing 5! with recursion:

```lisp
(letrec 
  [fact (lambda (x) 
    (cond 
      [(eq x 1) 1]
      [t (* x (fact (- x 1)))]))] 
  (fact 5))
; 120
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
 - [x] normal order (lazy evaluation)

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
 - [x] support quote `'` syntaxic sugar
 - [ ] support quasiquote + unquote
 - [ ] handle dot syntax `(a . b)`; ie. the case where `cdr(a)` is not NIL or a PAIR

### Special forms

 - [X] let
 - [x] lambda
   - [x] support for variadic functions (eg. `(lambda all-args (length all-args))` would return the number of passed args)
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
   - [ ] gensym to create unique symbols for macros
 - [x] Lisp functions / macros
   - [ ] recursivity
     - [x] Y combinator
     - [x] letrec
     - [ ] letrec* for mutual recursivity
   - [x] and / or
   - [x] cond
   - [x] map / fold
   - [ ] streams (lazy lists) / corecursivity
   - [ ] pattern matching
 - [x] Expose interpreter internals
   - [x] `sexpr` / `parse`
   - [x] `eval` / `apply`


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

 *I would like to see if we can evolve, in the future, this interpreter as a Continuation Passing Style interpreter.
 Cyclone Scheme may be an inspiration. It could also help me going closer to the metal. Maybe.*

### Bytecode Interpreter

*Just a distant dream for now...*

## Other limitations...

...that come to my mind

 - Macro should be resolved ahead of time, in a dedicated pass IMO
 - Native functions are awful
   - The proper way would be to have one type only
   - It would support variadic functions
   - It would pass the whole interpreter environment for fun & profit (including env, store, etc.)
   - It would require an update of the apply() C call
 - Env management with the assoc list is very inefficient
   - It would be solved at the same time as we implement the lexical scoping optimization
   - It's weird we register the global functions in the base env; it should be in the store
   - But ideally, I would prefer we have no store at all, and everything to be lexically scoped
 - Vulnerable to stack overflow
   - Sure TCO (Tail Call Optimization) is an option
   - I might be wrong but I think CPS (Continuation Passing Style) interprets could solve that too...
 - No error management (program just crash)
   - Could be done properly with call/cc
 - No static type
   - I am a bit lost on this topic; I've seen Typed Scheme but I don't understand it really...
 - No proper check of compound argument number leading to weird bug.
   - eg. `((lambda (a b) (+ a b)) 3)` (missing `b` binding) leads to a weird error.

## Resources

What motivated me to start this project: [ William Byrd on "The Most Beautiful Program Ever Written" [PWL NYC] ](https://www.youtube.com/watch?v=OyfBQmvr2Hc)

I've read (at least partially):
 - Crafting Interpreters by Robert Nystrom
 - SICP by H. Abelson, G. J. Sussman and J. Sussman

On my reading list:
 - Lisp In Small Pieces by C. Queinnec
 - Paradigms of Artificial Intelligence Programming (PAIP) by P. Norvig
