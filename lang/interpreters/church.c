#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MALLOC_FAILED 13

// ---- Generic datastructures

struct list {
  void* value;
  struct list* next;
};

struct list* cons(void* head, struct list* rest) {
  struct list* cell = malloc(sizeof(struct list));
  if (cell == NULL) {
    exit(MALLOC_FAILED);
  }
  cell->value = head;
  cell->next = rest;
  return cell;
}

void* head(struct list* cell) {
  return cell->value;
}

struct list* rest(struct list* cell) {
  return cell->next;
}

bool empty(struct list* cell) {
  return cell == NULL;
}

// --- Lexer

enum token_type {
  LEFT_PAREN,
  RIGHT_PAREN,
  DEFINE,
  LAMBDA,
  SYMBOL,
};

struct token {
  enum token_type type;
  char* symbol;
};

int main(void) {
  struct list* r;
  int a = 5;
  int b = 6;
  int c = 9;
  r = cons(&a, NULL);
  r = cons(&b, r);
  r = cons(&c, r);

  while (!empty(r)) {
    int* v = head(r);
    r = rest(r);
    printf("val: %d\n", *v);
  } 

  return 0;
}
