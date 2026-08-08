#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "errors.h"
#include "mem.h"
#include "symbols.h"

/*
 * SHARED REFS
 */
#define ASCII_CODE_ZERO 48
#define ASCII_CODE_NINE 57
#define ASCII_CODE_EXCLAMATION 33
#define ASCII_CODE_TILDE 126


/*
 * DATATYPES PRIMITIVES
 */

//   c-bindings
atom* cbool(int b);
int boolc(atom* a);
atom* cnumber(int64_t v);
string_t* heap_string(char* s, size_t len);
atom* cstring(char* s, size_t len);
int  cstring_eq(string_t* s1, string_t* s2);
atom* csymbol(char* s);
void symbols_free();

//   lisp-compatible
atom* nil();
atom* _false(); // false is nil
atom* _true(); // true is symbol t
atom* weak(atom* orig); // build a weak pointer to break circular references
atom* cons(atom* left, atom* right); // build a pair (or extend a list)
atom* cdr(atom* list); // 2nd element of a pair (or rest of list)
atom* nth(atom* list, int pos);
atom* car(atom* list); // 1st element of a pair (or head of list)
atom* cadr(atom* list); // 2nd element of a list
atom* caddr(atom* list); // 3rd element of a list
atom* cadddr(atom* list); // 4th element of a list
atom* empty(atom* a); // if list is empty
atom* not(atom* a); // not. nil becomes t; anything else becomes nil.
atom* length(atom* list); // length of a list
atom* reverse(atom* list); // reverse a list
atom* eq(atom* a1, atom* a2); // test 2 atoms for equality
atom* number(atom* charlist); // build a number from a list of char
atom* plus(atom* a1, atom* a2);
atom* minus(atom* a1, atom* a2);
atom* mult(atom* a1, atom* a2);
atom* divi(atom* a1, atom* a2);
atom* mod(atom* a1, atom* a2);
atom* gt(atom* a1, atom* a2);
atom* lt(atom* a1, atom* a2);
atom* ge(atom* a1, atom* a2);
atom* le(atom* a1, atom* a2);
atom* string(atom* charlist); // build a string from a list of char
atom* string_concatenate(atom* a1, atom* a2); // concatenate 2 strings
atom* symbol(atom* a); // build an atom from a string
atom* sexpr(atom* a); // build a string atom representing any atom (including list/pair) as a sexpr
atom* debug_sexpr(atom* a); // build a string atom representing any atom (including list/pair) as a sexpr
atom* print(atom* a);

/*
 * LEXER
 */
atom* lex(FILE* f);

/*
 * PARSER
 */
atom* expr(atom* lex);

/*
 * INTERPRETER
 */

// force_it is required in the standard lib.
// it is also required in the repl.
// it's ugly.
atom* force_it(atom* a); 

atom* eval(atom* ast, atom* env);
atom* full_env();


#endif
