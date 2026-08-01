# LispDub

A lisp as cringe as your favorite 2010 lip dub clip.

## Features

### Memory Management

- [X] Reference Counting
- [ ] Arena allocation (eg. 512 atoms for ~12KiB arenas)
- [ ] Copy object when RC reaches MAX_INT
- [ ] (maybe) Weak pointers

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

 - [X] s-expr lexer
   - [ ] handle dot syntax `(foo . bar)`
 - [x] s-expr parser
   - [ ] handle dot syntax `(foo . bar)`
 - [x] s-expr serializer
   - [ ] handle cases where `cdr(a)` is not NIL or a PAIR

### Control flow
 - [X] let
 - [ ] letrec
 - [x] lambda
 - [ ] define

### Standard Library
 - [x] C functions
   - [x] boolean logic (`if`)
   - [x] arithmetic (`+`, `-`, `*`, `/`)


### Tree-Walk Interpreter

### Bytecode compiler

 - [ ] Bytecode definition
 - [ ] Bytecode emission
 - [ ] Optimizations
   - [ ] RC incr/decr peephole optimizer

### VM

*TODO*

## Other limitations

...that come to my mind

 - No TCO
 - No static type
