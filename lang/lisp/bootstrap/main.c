#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MALLOC_ERR 100
#define MALLOC_MSG "Malloc failed"
#define SYMB_TOO_LONG_ERR 101
#define SYMB_TOO_LONG_MSG "symbol can be up to 32 chars"
#define SYMB_EMPTY_ERR 102
#define SYMB_EMPTY_MSG "symbol must be at least 1 char"

/****
 * ERROR
 ****/
void error(int code, char* msg) {
  fprintf(stderr, "error: %s\n", msg);
  exit(code);
}

/****
 * DATATYPES
 ****/

// -- string
typedef struct string {
  size_t len;
  char content[];
} string_t;

string_t* string(char* s, int len) {
  string_t* ptr = malloc(sizeof(string_t)+sizeof(char)*len);
  if (ptr == NULL) error(MALLOC_ERR, MALLOC_MSG);
  ptr->len = len;
  strncpy(ptr->content, s, len);
  return ptr;
}

void string_free(string_t* ptr) {
  free(ptr);
}

// -- symbols
#define SYMBOL_MAX_LEN 32
#define SYMBOL_EXTEND 10
typedef struct symb_repo {
  size_t sz;
  size_t cur;
  unsigned char* buf;
} symb_repo;

symb_repo* new_symb_repo() {
  symb_repo* res = malloc(sizeof(symb_repo));
  if (res == NULL) error(MALLOC_ERR, MALLOC_MSG);
  size_t max_str = sizeof(string_t)+sizeof(char)*SYMBOL_MAX_LEN;
  res->sz = max_str*SYMBOL_EXTEND;
  res->cur = 0;
  res->buf = malloc(res->sz);
  if (res->buf == NULL) error(MALLOC_ERR, MALLOC_MSG);
  memset(res->buf, 0, res->sz);
  return res;
}

short symbol(symb_repo* sr, char* s, int len) {
  if (len <= 0) error(SYMB_EMPTY_ERR, SYMB_EMPTY_MSG);
  if (len > SYMBOL_MAX_LEN) error(SYMB_TOO_LONG_ERR, SYMB_TOO_LONG_MSG);

  short counter = 0;
  size_t cursor = 0;

  // try to find
  while (cursor < sr->cur) {
    counter += 1;
    string_t* cand = (string_t*)&sr->buf[cursor];
    if (cand->len == len && strncmp(cand->content, s, len) == 0) {
      return counter; // found
    }
    cursor += sizeof(string_t) + sizeof(char)*cand->len;
  }

  // make sure we have enough memory
  size_t max_str = sizeof(string_t)+sizeof(char)*SYMBOL_MAX_LEN;
  if (sr->sz - sr->cur < max_str+1) {
    sr->sz = sr->sz + SYMBOL_EXTEND * max_str + 1;
    sr->buf = realloc(sr->buf, sr->sz);
    if (sr->buf == NULL) error(MALLOC_ERR, MALLOC_MSG);
  }

  // copy
  string_t* new_symbol = (string_t*)&sr->buf[sr->cur];
  new_symbol->len = len;
  strncpy(new_symbol->content, s, len);
  sr->cur = sr->cur + sizeof(string_t) + sizeof(char) * len;
  return counter+1;
}


// NIL = special symbol.
// false = NIL ; true = everything else
#define SYMBOL 1
#define INT64 2
#define STRING 3

typedef struct atom {
  unsigned char kind;
  union {
    int numeric;
    short atom;
    string_t* text;
  } val;
} atom;

int main(void) {
  symb_repo* sr = new_symb_repo();
  short a = symbol(sr, "hello", 6);
  short b = symbol(sr, "world", 6);
  short c = symbol(sr, "hello", 6);
  printf("%d ; %d ; %d\n", a, b, c);
  return 0;
}
