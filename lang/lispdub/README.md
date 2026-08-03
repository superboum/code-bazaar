# LispDub

A lisp as cringe as your favorite 2010 lip dub clip.

## Quickstart

```bash
gcc -Wall ./main.c
./a.out
```

## Features

### Memory Management

- [X] Reference Counting
- [ ] Arena allocation (eg. 512 atoms for ~12KiB arenas)
  - [ ] Free empty arenas
  - [ ] Compact arenas
- [ ] Copy object when RC reaches MAX_INT
- [ ] Weak references for recursivity

### Datatype

Our language has a single datastructure: atoms.
It is designed to fit on 24 bytes.
 - [X] public atoms
   - [x] nil
   - [x] symbols
   - [X] int64
   - [x] strings
   - [ ] short strings (encoded on 16 bytes; 15 characters)
   - [x] pairs
 - [x] runtime atoms
   - [x] closure (lambda + env)
   - [x] fx1, fx2, fx3 for C function bindings

### S-expr serialization / deserialization

 - [x] s-expr lexer
   - [ ] handle dot syntax `(foo . bar)`
 - [x] s-expr parser
   - [ ] handle dot syntax `(foo . bar)`
 - [x] s-expr serializer
   - [ ] handle cases where `cdr(a)` is not NIL or a PAIR

### Special forms

 - [X] let
 - [x] lambda
 - [x] quote
 - [ ] thunk (delayed execution / normal order)
   - [ ] Memoized thunks
 - [ ] define
 - [ ] macro
 - [ ] quasiquote + unquote

 *Note: for now, conditionals are implemented as a function*

### Standard Library

 - [x] C functions
   - [x] boolean logic (`if`)
   - [x] arithmetic (`+`, `-`, `*`, `/`)
   - [ ] list processing
     - [x] `reverse`
     - [ ] basic utilities like `car`, `cdr`, `cons`, etc.
   - [ ] string processing
 - [ ] Lisp functions (don't know yet how I will handle that)


### Optimizations
 - [ ] Lexical Addressing (or Static Scope Resolution)

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
 - No TCO
 - No static type

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
 - Crafting Interpreters by Robert Nystrom
 - SICP 
