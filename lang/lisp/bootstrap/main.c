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
#define ERR_RC_ERROR_CODE 104
#define ERR_RC_ERROR_MSG "reference counting logic error; object is freed."

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

struct atom;
typedef struct pair {
  struct atom* head;
  struct atom* tail;
} pair;

typedef struct atom {
  char kind;
  char rc;
  union {
    int as_number;
    string_t* as_string;
    struct pair as_pair;
  } val;
} atom;




// datatype interface
//   c-bindings
atom* atom_alloc();
atom* atom_rc_incr(atom* a);
atom* atom_rc_decr(atom* a);
atom* cbool(int b);
int boolc(atom* a);
atom* cnumber(int v);
atom* cstring(char* s, size_t len);
int  cstring_eq(string_t* s1, string_t* s2);
atom* csymbol(char* s);

//   lisp-compatible
atom* nil();
atom* _false(); // false is nil
atom* _true(); // true is symbol t
atom* cons(atom* left, atom* right); // build a pair (or extend a list)
atom* car(atom* list); // 1st element of a pair (or head of list)
atom* cdr(atom* list); // 2nd element of a pair (or rest of list)
atom* empty(atom* a); // if list is empty
atom* not(atom* a); // not. nil becomes t; anything else becomes nil.
atom* length(atom* list); // length of a list
atom* reverse(atom* list); // reverse a list
atom* eq(atom* a1, atom* a2); // test 2 atoms for equality
atom* number(atom* charlist); // build a number from a list of char
atom* string(atom* charlist); // build a string from a list of char
atom* string_concatenate(atom* a1, atom* a2); // concatenate 2 strings
atom* symbol(atom* a); // build an atom from a string
atom* sexpr(atom* a); // build a string atom representing any atom (including list/pair) as a sexpr

atom* atom_alloc() {
  atom* ptr = malloc(sizeof(atom));
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  memset(ptr, 0, sizeof(atom));
  atom_rc_incr(ptr);
  ptr->kind = NIL;
  return ptr;
}

atom* atom_rc_incr(atom* a) {
  if (a == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  if (a->rc < 255) a->rc++;
  return a;
  // we don't need to increment recursively.
  // instead, children rc is incremented only when attached to parent.
}

atom* atom_rc_decr(atom* a) {
  if (a == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  if (a->rc != 255) a->rc--;
  if (a->rc == 0) {
    if (a->kind == PAIR) {
      a->val.as_pair.head = atom_rc_decr(a->val.as_pair.head);
      a->val.as_pair.tail = atom_rc_decr(a->val.as_pair.tail);
    }
    if (a->kind == STRING || a->kind == SYMBOL) {
      // theoretically, symbols are never freed as their rc never reach zero
      // as they always registered in the global symbol index...
      free(a->val.as_string);
      a->val.as_string = NULL;
    }
    free(a);
    return NULL;
  }
  return a;
}

atom* _nil = NULL;
atom* nil() {
  if (_nil == NULL) {
    _nil = atom_alloc();
    _nil->kind = NIL;
    _nil->rc = 255; // disable rc
  }
  return _nil;
}

atom* _false() {
  return nil();
}

atom* __true = NULL;
atom* _true() {
  if (__true == NULL) {
    __true = csymbol("t");
    __true->rc = 255; // disable rc
  }
  return __true;
}
atom* cbool(int b) {
  if(b) return _true();
  return _false();
}
int boolc(atom* a) {
  if (a->kind == NIL) return 0;
  return 1;
}

atom* cons(atom* left, atom* right) {
  // left & right are now owned by the result; so we decrement as we consume, and increment as we bind to the new struct
  if (left == NULL || right == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  atom* a = atom_alloc();
  a->kind = PAIR;
  a->val.as_pair.head = atom_rc_incr(left);
  a->val.as_pair.tail = atom_rc_incr(right);
  return a;
}
atom* car(atom* list) {
  if (list == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  if (list->kind != PAIR) error(ERR_CANT_CAR_CODE, ERR_CANT_CAR_MSG);
  
  return atom_rc_incr(list->val.as_pair.head);
}
atom* cdr(atom* list) {
  if (list == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  if (list->kind != PAIR) error(ERR_CANT_CAR_CODE, ERR_CANT_CAR_MSG);
  
  return atom_rc_incr(list->val.as_pair.tail);;
}
atom* empty(atom* a) {
  // NO RC with bools
  if (a == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  atom* r = _false();
  if (a->kind == NIL) r = _true();

  return r;
}
atom* not(atom* a) {
  // NO RC with bools
  return empty(a);
}

atom* length(atom* list) {
  if (list == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  int len = 0;
  while (boolc(list)) {
    len++;
    list = atom_rc_decr(cdr(list)); // protected by list root
  }

  return cnumber(len);
}

atom* reverse(atom* list) {
  if (list == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  atom* acc = nil();
  while (boolc(list)) {
    atom* head = atom_rc_decr(car(list)); // protected by list root
    atom* new_acc = cons(head, acc);
    atom_rc_decr(acc); // old accumulator is not required anymore
    acc = new_acc;
    list = atom_rc_decr(cdr(list)); // protected by list root
  }
  return acc;
}

int cstring_eq(string_t* s1, string_t* s2) {
    if (s1->len != s2->len) return 0;
    if (strncmp(s1->val, s2->val, s1->len) != 0) return 0;
    return 1;
}

atom* eq(atom* a1, atom* a2) {
  if (a1 == NULL || a2 == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);

  // by default it's false
  atom* res = _false();
  if (a1->kind != a2->kind) res = _false(); // we don't cast transparently, so trap all type differences here...
  else if (a1->kind == NUMBER) res = cbool(a1->val.as_number == a2->val.as_number); // compare values
  else if (a1->kind == SYMBOL) res = cbool(a1->val.as_string == a2->val.as_string); // compare mem addr as symbols deduplicate strings
  else if (a1->kind == PAIR) res = cbool(a1 == a2); // compare mem addr
  else if (a1->kind == STRING) res = cbool(cstring_eq(a1->val.as_string, a2->val.as_string)); // compare with strcnmp
  else if (a1->kind == NIL) res = _true(); // nil is always equal to nil
  return res;
}

atom* cnumber(int v) {
  atom* a = atom_alloc();
  a->kind = NUMBER;
  a->val.as_number = v;
  return a;
}

atom* number(atom* charlist) {
  // @FIXME: numbers as a list of char. Reverse then 10*(converted)+acc
  atom* a = atom_alloc();
  a->kind = NUMBER;
  a->val.as_number = 0;
  return a;
}

atom* cstring(char* s, size_t len) {
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  memset(ptr, 0, memsz);
  ptr->len = len;
  strncpy(ptr->val, s, len);

  atom* a = atom_alloc();
  a->kind = STRING;
  a->val.as_string = ptr;
  return a;
}

atom* string(atom* charlist) {
  if (charlist == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  atom* alen = length(charlist);
  size_t len = alen->val.as_number;
  alen = atom_rc_decr(alen); // we don't need alen past this point

  // build string
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  memset(ptr, 0, memsz); // make sure we initialize with zero
  ptr->len = len;
  for (size_t i = 0; i < len; i++) {
    atom* acharcode = atom_rc_decr(car(charlist)); // protected by list root
    if (acharcode->kind != NUMBER) error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG);
    int charcode = acharcode->val.as_number;
    if (charcode < 0 || charcode > 255) error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG);
    ptr->val[i] = charcode;
    charlist = atom_rc_decr(cdr(charlist)); // protected by list root
  }

  atom* res = atom_alloc();
  res->kind = STRING;
  res->val.as_string = ptr;
  return res;
}

atom* string_concatenate(atom* a1, atom* a2) {
  if (a1 == NULL || a2 == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  if (a1->kind != STRING && a2->kind != STRING) error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG);

  // build new string
  size_t len = a1->val.as_string->len + a2->val.as_string->len;
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  memset(ptr, 0, memsz);
  ptr->len = len;
  strncpy(ptr->val, a1->val.as_string->val, a1->val.as_string->len);
  strncpy(ptr->val+a1->val.as_string->len, a2->val.as_string->val, a2->val.as_string->len);

  // build new atom
  atom* a = atom_alloc();
  a->kind = STRING;
  a->val.as_string = ptr;
  return a;
}

atom* global_symbols = NULL;
atom* symbol(atom* a) {
  // NOTE: do not use Lisp boolean heres as true is defined as a symbol
  if (a == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  if (a->kind == SYMBOL) return a;
  if (global_symbols == NULL) global_symbols = nil();
  if (a->kind != STRING) error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG);
  string_t* inner = a->val.as_string;

  // Try to find symbol
  atom* iter = global_symbols;
  while(iter->kind != NIL) {
    atom* cur = atom_rc_decr(car(iter)); // protected by list root
    iter = atom_rc_decr(cdr(iter)); // protected by list root
    if (cur->kind != SYMBOL) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG); // wrong type
    string_t* cur_str = cur->val.as_string;
    if (cur_str->len != inner->len) continue; // length does not match
    if (strncmp(cur_str->val, inner->val, inner->len) != 0) continue; // chars do not match
    return atom_rc_incr(cur); // we make a "copy" for the outside.
  }

  // Symbol not found, creating it (and trigger an allocation)
  atom* new_symbol = cstring(inner->val, inner->len);
  new_symbol->kind = SYMBOL;
  global_symbols = cons(new_symbol, global_symbols);
  return new_symbol;
}
atom* csymbol(char* s) {
  return symbol(cstring(s, strlen(s)));
}

atom* sexpr(atom* a) {
  if (a->kind == NIL) return cstring("NIL", 3);
  if (a->kind == SYMBOL) return cstring(a->val.as_string->val, a->val.as_string->len);
  if (a->kind == STRING) return a; // @FIXME add double quotes
  if (a->kind == NUMBER) {
    
  }

  return a;
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
atom* lex_string(FILE* f) {
  atom* acc = nil();
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || c == '"') break;
    atom* anumber = cnumber(c);
    atom* new_acc = cons(anumber, acc);
    atom_rc_decr(anumber);
    atom_rc_decr(acc);
    acc = new_acc;
  }
  atom* value = string(acc);
  atom* part2 = cons(value, nil());
  atom* type = csymbol("string");
  atom* final = cons(type, part2);
  atom_rc_decr(acc);
  atom_rc_decr(type);
  atom_rc_decr(value);
  atom_rc_decr(part2);
  return final;
}

// Returns a (symbol foo)
atom* lex_symbol(FILE* f) {
  atom* acc = nil();
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || !(is_symbol(c))) {
      ungetc(c, f);
      break;
    }
    atom* anumber = cnumber(c);
    atom* new_acc = cons(anumber, acc);
    atom_rc_decr(anumber);
    atom_rc_decr(acc);
    acc = new_acc;
  }
  atom* parsed_string = string(acc);
  atom* final_symbol = symbol(parsed_string);
  atom* part2 = cons(final_symbol, nil());
  atom* type = csymbol("symbol");
  atom* final = cons(type, part2);
  atom_rc_decr(acc);
  atom_rc_decr(parsed_string);
  atom_rc_decr(final_symbol);
  atom_rc_decr(part2);
  atom_rc_decr(type);
  return final;
}

// Returns a (number 67)
atom* lex_number(FILE* f) {
  atom* acc = nil();
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || !(is_digit(c))) {
      ungetc(c, f);
      break;
    }
    atom* anumber = cnumber(c);
    atom* new_acc = cons(anumber, acc);
    atom_rc_decr(anumber);
    atom_rc_decr(acc);
    acc = new_acc;
  }
  atom* anum = number(acc);
  atom* part2 = cons(anum, nil());
  atom* type = csymbol("number");
  atom* final = cons(type, part2);
  atom_rc_decr(acc);
  atom_rc_decr(anum);
  atom_rc_decr(part2);
  atom_rc_decr(type);
  return final;
}

// Returns a token. (lparen) | (rparen) | (number 67) | (symbol foo) | (string "blabla")
atom* lex_token(FILE* f) {
  // the loop eats spaces & new lines
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF) return nil();
    if (c == '(') {
	atom* type = csymbol("lparen");
	atom* final = cons(type, nil());
	atom_rc_decr(type);
	return final;
    }
    if (c == ')') {
	atom* type = csymbol("rparen");
	atom* final = cons(type, nil());
	atom_rc_decr(type);
	return final;
    }
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
  atom* s1 = csymbol("hello");
  atom* s2 = csymbol("world");
  atom* s3 = csymbol("hello");
  if (boolc(eq(s1,s2))) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  if (boolc(not(eq(s1, s3)))) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  if (boolc(eq(s2, s3))) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  atom_rc_decr(s1);
  atom_rc_decr(s2);
  atom_rc_decr(s3);

  atom* c1 = cstring("hello", 5);
  atom* c2 = cstring(" world\n", 7);
  atom* c3 = string_concatenate(c1, c2);
  printf("concatenated: %s", c3->val.as_string->val);
  c1 = atom_rc_decr(c1);
  c2 = atom_rc_decr(c2);
  c3 = atom_rc_decr(c3);
  if (c1 != NULL || c2 != NULL || c3 != NULL) exit(512);

  printf("all good\n");
  return 0;
}
