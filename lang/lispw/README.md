# LispW

A lisp as cringe as your favorite 2010 lip dub clip (did you get the joke?).

Example of computing 5! with recursion:

```lisp
(let (fact (lambda (x) 
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

 - [x] lexical scoping (define may be the exception here, not sure yet...)
 - [x] recursivity, corecursivity (through define only)
 - [x] immutability
 - [ ] implicit progn (only) on let / lambda / etc.
 - [x] few syntactic sugar
 - [x] *should be* mostly normal order (and not applicative order, not sure yet...)

### Memory Management

- [x] Reference Counting
- [x] Slab allocator
- [x] Weak references for recursivity

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

 - [x] s-expr lexer
   - [x] support comments
 - [x] s-expr parser
 - [x] s-expr serializer
 - [ ] handle dot syntax `(a . b)`; ie. the case where `cdr(a)` is not NIL or a PAIR

### Special forms

 - [X] let (with recursivity)
 - [x] lambda
 - [x] quote
 - [x] if
 - [x] thunk (delayed execution / normal order)
 - [x] define (trough a hack however)
   - [ ] properly scoped define 
 - [ ] macro
 - [ ] quasiquote + unquote

### Standard Library

 - [x] C functions
   - [x] boolean logic (`if`)
   - [x] arithmetic (`+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`)
   - [x] list processing
     - [x] `reverse`
     - [x] basic utilities like `car`, `cdr`, `cons`, etc.
   - [ ] string processing
 - [x] Lisp functions / macros
   - [x] and / or
   - [ ] cond
   - [ ] map / fold
   - [ ] streams (lazy lists)
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

 - [x] basic functionalities
 - [ ] proper REPL

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

I've read (at least partially):
 - Crafting Interpreters by Robert Nystrom
 - SICP by H. Abelson, G. J. Sussman and J. Sussman

On my reading list:
 - Lisp In Small Pieces by C. Queinnec
 - Paradigms of Artificial Intelligence Programming (PAIP) by P. Norvig
