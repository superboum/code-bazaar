#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MALLOC_FAILED 13

// ---- Generic datastructures
struct string {
  size_t len;
  char* buffer;
};

struct string* str_box(struct string* content) {
  struct string* ptr = malloc(sizeof(size_t)+sizeof(char*));
  if (ptr == NULL) exit(MALLOC_FAILED);
  ptr->len = content->len;
  ptr->buffer = malloc(sizeof(char)*content->len);
  if (ptr->buffer == NULL) exit(MALLOC_FAILED);
  strncpy(ptr->buffer, content->buffer, content->len);
  return ptr;
}

int str_eq(struct string* a, struct string* b) {
  if (a->len != b->len) return 1;
  if (strncmp(a->buffer, b->buffer, a->len) != 0) return 1;
  return 0;
}

void str_print(struct string* this) {
  fwrite(this->buffer, sizeof(char), this->len, stdout);
}

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

struct symbol_res {
  struct list* symbol_env;
  struct string* symbol_ref;
};

struct symbol_res symbol(struct list* symbol_env, struct string* name) {
  struct list* iter = symbol_env;
  while (iter != NULL) {
    if (str_eq(iter->value, name) == 0) {
      // symbol already exists
      struct symbol_res res = { symbol_env, iter->value };
      return res;
    }
    iter = iter->next;
  }

  // symbol added
  struct string* new_ref = str_box(name);
  struct list* new_env = cons(new_ref, symbol_env);
  struct symbol_res res = { new_env, new_ref };
  return res;
}

// --- Lexer

int main(void) {
  /*struct list* r;
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
  }*/

  struct string test1 = { 5, "HELLO" };
  struct string test2 = { 5, "HELLO" };

  struct list* env1 = NULL;
  struct symbol_res res1 = symbol(env1, &test1);
  struct list* env2 = res1.symbol_env;
  struct symbol_res res2 = symbol(env2, &test2);
  struct list* env3 = res2.symbol_env;
  if (env1 == env2) exit(100);
  if (env2 != env3) exit(101);
  str_print(&test1);

  return 0;
}
