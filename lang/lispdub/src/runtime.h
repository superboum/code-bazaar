#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

/*
 * ERROR MANAGEMENT
 */
#define ERR_MALLOC_CODE 100
#define ERR_MALLOC_MSG "Not enough memory."
#define ERR_LOGIC_CODE 101
#define ERR_LOGIC_MSG "Internal logic error."
#define ERR_ATOM_WRONG_TYPE_CODE 102
#define ERR_ATOM_WRONG_TYPE_MSG "An atom of a given type was expected; something else was found."
#define ERR_CANT_CAR_CODE 103
#define ERR_CANT_CAR_MSG "can't car or cdr this atom as it's not a pair."
#define ERR_RC_ERROR_CODE 104
#define ERR_RC_ERROR_MSG "reference counting logic error; object is freed."
#define ERR_PARSER_ERROR_CODE 105
#define ERR_PARSER_ERROR_MSG "Parser failed. You probably have a syntax error in your code"
#define ERR_INTERPRETER_CODE 106
#define ERR_INTERPRETER_MSG "Interpreter failed. Check your syntax."
#define ERR_LEAK_CODE 107
#define ERR_LEAK_MSG "Memory leak detected: some allocated objects were not deallocated."
#define ERR_UNDEFINED_CODE 108
#define ERR_UNDEFINED_MSG "Tried to resolve a variable that does not exist."

void error(int code, char* msg);

/*
 * SHARED REFS
 */
#define ASCII_CODE_ZERO 48
#define ASCII_CODE_NINE 57
#define ASCII_CODE_EXCLAMATION 33
#define ASCII_CODE_TILDE 126


/*
 * DATATYPES DEFINITION
 */
#define ERROR  0
#define NUMBER 1
#define STRING 2
#define SYMBOL 3
#define PAIR   4
#define CLOSU  5
#define THUNK  6
#define FX1    7
#define FX2    8
#define FX3    9
#define WEAK   10
#define NIL    11

typedef struct string {
  size_t len;
  char val[];
} string_t;

struct atom;

typedef struct atom* (*fx1)(struct atom*);
typedef struct atom* (*fx2)(struct atom*, struct atom*);
typedef struct atom* (*fx3)(struct atom*, struct atom*, struct atom*);

typedef struct pair {
  struct atom* head;
  struct atom* tail;
} pair;

typedef struct closu {
  struct atom* expr;
  struct atom* env;
} closu; // same as pair...

typedef struct atom {
  char kind;
  short rc;
  union {
    int as_number;
    string_t* as_string;
    struct pair as_pair;
    struct closu as_capture;
    struct atom* as_weak;
    fx1 as_fx1;
    fx2 as_fx2;
    fx3 as_fx3;
  } val;
} atom;

/*
 * MEMORY MANAGEMENT
 */

#define RC_DISABLED_DUE_TO_STATIC_ALLOC -1
atom* atom_alloc(char kind);
atom* atom_rc_incr(atom* a);
atom* atom_rc_decr(atom* a);
void  rc_stats(void);
void  rc_memleak_check(void);

/*
 * DATATYPES PRIMITIVES
 */

//   c-bindings
atom* cbool(int b);
int boolc(atom* a);
atom* cnumber(int v);
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
void print(atom* a);

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
