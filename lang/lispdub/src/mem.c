#include "runtime.h"

/*
 * MEMORY MANAGEMENT
 */
size_t allocated_objects = 0;
size_t alloc_count_per_kind[32] = {0};

const int atom_kind_count = 12;
const char* atom_kind_names[12] = {
  "ERROR",
  "NUMBER",
  "STRING",
  "SYMBOL",
  "PAIR",
  "CLOSU",
  "THUNK",
  "FX1",
  "FX2",
  "FX3",
  "WEAK",
  "NIL",
};

atom* atom_alloc(char kind) {
  atom* ptr = malloc(sizeof(atom));
  if (ptr == NULL) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG);
  memset(ptr, 0, sizeof(atom));
  atom_rc_incr(ptr);
  ptr->kind = kind;
  allocated_objects++;
  alloc_count_per_kind[(int)kind]++;
  return ptr;
}

atom* atom_rc_incr(atom* a) {
  if (a == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  //if (a->rc >= SHRT_MAX) fprintf(stderr, "RC reached\n");
  if (a->rc >= SHRT_MAX) {
    fprintf(stderr, "RC_MAX reached. Leaking memory now.\n");
    //@FIXME: symbols do not work well with memory copy. Here are the known problems:
    // 1. Equality is computed based on pointer address. But now that we copy, it's no longer working.
    // 2. For a reason I don't understand yet, the if symbol is freed twice in the eval function when the copy code is called.
    /*atom* new = malloc(sizeof(atom));
    new->kind = a->kind;
    new->rc = 1;
    new->val = a->val;
    a = new;*/
  } else if (a->rc >= 0) {
    a->rc++;
  } else {
    // Counter is negative; it means it is disabled for this atom.
  }
  return a;
  // we don't need to increment recursively.
  // instead, children rc is incremented only when attached to parent.
}

atom* atom_rc_decr(atom* a) {
  if (a == NULL) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
  if (a->rc > 0) a->rc--;
  if (a->rc == 0) {
    /*printf("will free: ");
    print(sexpr(a));*/
    if (a->kind == PAIR) {
      a->val.as_pair.head = atom_rc_decr(a->val.as_pair.head);
      a->val.as_pair.tail = atom_rc_decr(a->val.as_pair.tail);
    }
    if (a->kind == CLOSU || a->kind == THUNK) {
      a->val.as_capture.expr = atom_rc_decr(a->val.as_capture.expr);
      a->val.as_capture.env  = atom_rc_decr(a->val.as_capture.env);
    }
    if (a->kind == STRING || a->kind == SYMBOL) {
      // theoretically, symbols are never freed as their rc never reach zero
      // as they always registered in the global symbol index...
      free(a->val.as_string);
      a->val.as_string = NULL;
    }
    if (allocated_objects <= 0) error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG);
    allocated_objects--;
    free(a);
    return NULL;
  }
  return a;
}

void rc_stats(void) {
  printf("-- alloc count per kind --\n");
  for (int i = 0; i < atom_kind_count; i++) {
    printf("  %s [%ld]\n", atom_kind_names[i], alloc_count_per_kind[i]);
  }

  printf("-- aggregated stats --\n");
  printf("  live objects: %ld\n", allocated_objects);
}

void rc_memleak_check(void) {
  if (allocated_objects > 0) {
    fprintf(stderr, "Tracked allocated objects: %ld\n", allocated_objects);
    error(ERR_LEAK_CODE, ERR_LEAK_MSG);
  }
}

