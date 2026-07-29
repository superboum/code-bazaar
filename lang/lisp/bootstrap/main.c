#include "stdlib.h"
#include "stdio.h"
#include "string.h"

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

void error(int code, char* msg) {
  fprintf(stderr, "Fatal Error. %s\n", msg);
  exit(code);
}

/*
 * DATATYPES
 */

#define NUMBER 1
#define STRING 2
#define SYMBOL 3
#define PAIR   4
#define NIL    5

typedef struct string {
  size_t len;
  char val[];
} string_t;

struct pair;
typedef struct atom {
  char kind;
  union {
    int as_number;
    string_t* as_string;
    struct pair* as_pair;
  } val;
} atom;

typedef struct pair {
  atom head;
  atom tail;
} pair;


// datatype interface
//   c-bindings
atom cbool(int b);
int boolc(atom a);
atom cnumber(int v);
atom cstring(char* s, size_t len);
int  cstring_eq(string_t* s1, string_t* s2);
atom csymbol(char* s);

//   lisp-compatible
atom _false(); // false is nil
atom _true(); // true is symbol t
atom cons(atom left, atom right); // build a pair (or extend a list)
atom car(atom list); // 1st element of a pair (or head of list)
atom cdr(atom list); // 2nd element of a pair (or rest of list)
atom empty(atom a); // if list is empty
atom not(atom a); // not. nil becomes t; anything else becomes nil.
atom length(atom list); // length of a list
atom reverse(atom list); // reverse a list
atom eq(atom a1, atom a2); // test 2 atoms for equality
atom number(atom charlist); // build a number from a list of char
atom string(atom charlist); // build a string from a list of char
atom symbol(atom a); // build an atom from a string

atom nil = { .kind = NIL, .val.as_number = 0 };
atom _false() {
  return nil;
}
atom _true() {
  return csymbol("t");
}
atom cbool(int b) {
  if(b) return _true();
  return _false();
}
int boolc(atom a) {
  if (a.kind == NIL) return 0;
  return 1;
}

atom cons(atom left, atom right) {
  pair* ptr = malloc(sizeof(pair));
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  ptr->head = left;
  ptr->tail = right;
  return (atom) { .kind = PAIR, .val.as_pair = ptr };
}
atom car(atom list) {
  if (list.kind != PAIR) error(ERR_CANT_CAR_CODE, ERR_CANT_CAR_MSG);
  return list.val.as_pair->head;
}
atom cdr(atom list) {
  if (list.kind != PAIR) error(ERR_CANT_CAR_CODE, ERR_CANT_CAR_MSG);
  return list.val.as_pair->tail;
}
atom empty(atom a) {
  if (a.kind == NIL) return _true();
  return _false();
}
atom not(atom a) {
  if (a.kind == NIL) return _true();
  return _false();
}

atom length(atom list) {
  int len = 0;
  while (boolc(not(empty(list)))) {
    len++;
    list=cdr(list);
  }
  return cnumber(len);
}

atom reverse(atom list) {
  atom acc = nil;
  while (boolc(not(empty(list)))) {
    acc = cons(car(list), acc);
    list = cdr(list);
  }
  return acc;
}

int cstring_eq(string_t* s1, string_t* s2) {
    if (s1->len != s2->len) return 0;
    if (strncmp(s1->val, s2->val, s1->len) != 0) return 0;
    return 1;
}

atom eq(atom a1, atom a2) {
  if (a1.kind != a2.kind) return _false(); // we don't cast transparently...
  if (a1.kind == NUMBER) return cbool(a1.val.as_number == a2.val.as_number); // compare values
  if (a1.kind == SYMBOL) return cbool(a1.val.as_string == a2.val.as_string); // compare mem addr as symbols deduplicate strings
  if (a1.kind == PAIR) return cbool(a1.val.as_pair == a2.val.as_pair); // compare mem addr
  if (a1.kind == STRING) return cbool(cstring_eq(a1.val.as_string, a2.val.as_string)); // compare with strcnmp
  if (a1.kind == NIL) return _true(); // nil is always equal to nil
  error(ERR_LOGIC_CODE, ERR_LOGIC_MSG); // oops we forgot to define an atom kind...
  return _false(); // should never be reached but gcc is not clever enough to detect that.
}

atom cnumber(int v) {
  return (atom) { .kind = NUMBER, .val.as_number = v };
}

atom number(atom charlist) {
  // @FIXME: numbers as a list of char. Reverse then 10*(converted)+acc
  return (atom) { .kind = NUMBER, .val.as_number = 0 };
}

atom cstring(char* s, size_t len) {
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  memset(ptr, 0, memsz);
  ptr->len = len;
  strncpy(ptr->val, s, len);

  return (atom) { .kind = STRING, .val.as_string = ptr };
}

atom string(atom charlist) {
  size_t len = length(charlist).val.as_number;

  // build string
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  memset(ptr, 0, memsz); // make sure we initialize with zero
  ptr->len = len;
  for (size_t i = 0; i < len; i++) {
    int charcode = car(charlist).val.as_number;
    if (charcode < 0 || charcode > 255) error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG);
    ptr->val[i] = charcode;
  }

  return (atom) { .kind = STRING, .val.as_string = ptr };
}

atom global_symbols = { .kind = NIL, .val.as_number = 0 };
atom symbol(atom a) {
  if (a.kind == SYMBOL) return a;
  if (a.kind != STRING) error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG);
  string_t* inner = a.val.as_string;

  // Try to find symbol
  atom iter = global_symbols;
  while(boolc(not(empty(iter)))) {
    atom cur = car(iter);
    iter = cdr(iter);
    if (cur.kind != SYMBOL) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
    if (boolc(not(eq(cur, a)))) continue;
    return cur;
  }

  // Symbol not found, creating it (and trigger an allocation)
  atom new_symbol = cstring(inner->val, inner->len);
  new_symbol.kind = SYMBOL;
  global_symbols = cons(new_symbol, global_symbols);
  return new_symbol;
}
atom csymbol(char* s) {
  return symbol(cstring(s, strlen(s)));
}


/*
 * LEXER
 *
 * We handwrite the lexer but in the spirit we use a Deterministic Finite Automation
 */
#define ASCII_CODE_ZERO 48
#define ASCII_CODE_NINE 57
int is_digit(char c) {
  return (c >= ASCII_CODE_ZERO && c <= ASCII_CODE_NINE);
}

#define ASCII_CODE_EXCLAMATION 33
#define ASCII_CODE_TILDE 126
int is_symbol(char c) {
  return (
    c >= ASCII_CODE_EXCLAMATION 
      && c <= ASCII_CODE_TILDE
      && c != ')'
      && c != '('
  );
}

// Returns a (string "foobar")
atom lex_string(FILE* f) {
  atom acc = nil;
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || c == '"') break;
    acc = cons(cnumber(c), acc);
  }
  return cons(csymbol("string"), cons(string(acc), nil));
}

// Returns a (symbol foo)
atom lex_symbol(FILE* f) {
  atom acc = nil;
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || !(is_symbol(c))) {
      ungetc(c, f);
      break;
    }
    acc = cons(cnumber(c), acc);
  }
  atom final_symbol = symbol(string(acc));
  return cons(csymbol("symbol"), cons(final_symbol, nil));
}

// Returns a (number 67)
atom lex_number(FILE* f) {
  atom acc = nil;
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || !(is_digit(c))) {
      ungetc(c, f);
      break;
    }
  }
  return cons(csymbol("number"), cons(number(acc), nil));
}

atom lex_token(FILE* f) {
  // the loop eats spaces & new lines
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF) return nil;
    if (c == '(') return cons(csymbol("lparen"), nil);
    if (c == ')') return cons(csymbol("rparen"), nil);
    if (c == '"') {
      return lex_string(f);
    }
    if (is_digit(c)) {
      ungetc(c, f);
      return lex_number(f);
    }
    if (is_symbol(c)) {
      ungetc(c, f);
      return lex_symbol(f);
    }
  }
}

int main(void) {
  printf("0\n");
  atom s1 = csymbol("hello");
  printf("1\n");
  atom s2 = csymbol("world");
  printf("2\n");
  atom s3 = csymbol("hello");
  printf("a\n");
  if (boolc(eq(s1,s2))) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  printf("b\n");
  if (boolc(not(eq(s1, s3)))) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  printf("c\n");
  if (boolc(eq(s2, s3))) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  printf("all good\n");
  return 0;
}
