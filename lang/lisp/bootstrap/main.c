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
#define NIL 4

typedef struct string {
  size_t len;
  char val[];
} string_t;

typedef struct atom {
  char kind;
  union {
    int as_number;
    string_t* as_string;
  } val;
} atom;

typedef struct cell {
  atom*        head;
  struct cell* rest;
} cell;

cell nil = (cell) { .head = NULL, .rest = NULL };
cell* cons(atom* head, cell* rest) {
  cell* ptr = malloc(sizeof(cell));
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  ptr->head = head;
  ptr->rest = rest;
  return ptr;
}
atom* car(cell* list) {
  return list->head;
}
cell* cdr(cell* list) {
  return list->rest;
}
int empty(cell* list) {
  return list == &nil;
}

atom* string(char* s, size_t len) {
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  memset(ptr, 0, memsz);
  ptr->len = len;
  strncpy(ptr->val, s, len);

  atom* at = malloc(sizeof(atom));
  if (at == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  at->kind = STRING;
  at->val.as_string = ptr;
  return at;
}

struct cell* global_atoms = &nil;
atom* symbol(char* s, size_t len) {
  // Try to find symbol
  cell* iter = global_atoms;
  while(!empty(iter)) {
    atom* cur = car(iter);
    iter = cdr(iter);
    if (cur->kind != SYMBOL) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
    if (len != cur->val.as_string->len) continue;
    if (strncmp(s, cur->val.as_string->val, len) != 0) continue;
    return cur;
  }

  // Symbol not found, creating it.
  atom* new_symbol = string(s, len);
  new_symbol->kind = SYMBOL;
  global_atoms = cons(new_symbol, global_atoms);
  return new_symbol;
}

int main(void) {
  atom* s1 = symbol("hello", 5);
  atom* s2 = symbol("world", 5);
  atom* s3 = symbol("hello", 5);
  if (s1 == s2) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  if (s1 != s3) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  if (s2 == s3) error(ERR_LOGIC_CODE, ERR_LOGIC_MSG);
  printf("all good\n");
  return 0;
}
