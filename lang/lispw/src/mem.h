#ifndef MEM_H
#define MEM_H

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdalign.h>
#include "errors.h"

#define RC_DISABLED_DUE_TO_STATIC_ALLOC -1

/*
 * DATATYPES DEFINITION
 */
#define FREED  0
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
    int64_t as_number;
    string_t* as_string;
    struct pair as_pair;
    struct closu as_capture;
    struct atom* as_weak;
    fx1 as_fx1;
    fx2 as_fx2;
    fx3 as_fx3;

    // For memory management when freed
    struct atom* as_slab_prev;
  } val;
} __attribute__((aligned(32))) atom;

/*
 * MEMORY MANAGEMENT
 */

atom* atom_alloc(char kind);
atom* atom_rc_incr(atom* a);
atom* atom_rc_decr(atom* a);
void  rc_stats(void);
void  rc_memleak_check(void);

#endif
