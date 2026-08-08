#include "mem.h"

/*
 * MEMORY MANAGEMENT
 */

// Some checks on memory alignment
#define ATOM_SIZE 32
static_assert(
  sizeof(atom) == ATOM_SIZE, 
  "Atom struct size exceeds 32 bytes."
);
static_assert(
  alignof(atom) == ATOM_SIZE, 
  "Atom alignment is not 32 bytes."
);

#define ATOMS_PER_SLAB 2048 // Should be around ~64KiB
typedef struct slab {
  struct slab* prev;
  struct atom cells[ATOMS_PER_SLAB];
} slab_t;

typedef struct allocator {
  slab_t* head;
  atom* free_list;
} allocator_t;

void allocator_grow(allocator_t* alloc) {
  // malloc a slab
  slab_t* new_slab = aligned_alloc(32, sizeof(slab_t));
  if (new_slab == NULL) 
    error(ERR_MALLOC_CODE, ERR_MALLOC_MSG,__func__, __FILE__, __LINE__);

  // update the linked list
  new_slab->prev = alloc->head;
  alloc->head = new_slab;

  // build the free list
  new_slab->cells[0].kind = FREED;
  new_slab->cells[0].val.as_slab_prev = NULL;
  for (int i = 1; i < ATOMS_PER_SLAB; i++) {
    new_slab->cells[i].kind = FREED;
    new_slab->cells[i].val.as_slab_prev = &(new_slab->cells[i-1]);
  }

  // point to the head of the free list
  alloc->free_list = &(new_slab->cells[ATOMS_PER_SLAB-1]);
}

atom* allocator_new(allocator_t* alloc) {
  if (alloc->free_list == NULL) allocator_grow(alloc);
  if (alloc->free_list == NULL) 
    error(ERR_SLAB_CODE, ERR_SLAB_MSG, __func__, __FILE__, __LINE__);

  atom* ptr = alloc->free_list;
  alloc->free_list = ptr->val.as_slab_prev;
  return ptr;
}

void allocator_free(allocator_t* alloc, atom* a) {
  a->kind = FREED;
  a->val.as_slab_prev = alloc->free_list;
  alloc->free_list = a;
}

size_t allocator_live_atoms(allocator_t* alloc) {
  size_t allocated_objects = 0;
  slab_t* iter = alloc->head;
  while (iter != NULL) {
    for (int i = 0; i < ATOMS_PER_SLAB; i++) {
      if (iter->cells[i].kind != FREED) allocated_objects++;
    }
    iter = iter->prev;
  }
  return allocated_objects;
}

size_t allocator_slabs_count(allocator_t* alloc) {
  size_t slab_count = 0;
  slab_t* iter = alloc->head;
  while (iter != NULL) {
    slab_count++;
    iter = iter->prev;
  }
  return slab_count;
}

allocator_t global_allocator = {0};

size_t alloc_count_per_kind[32] = {0};

const int atom_kind_count = 13;
const char* atom_kind_names[13] = {
  "FREED",
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
  "MACRO",
  "NIL",
};

atom* atom_alloc(char kind) {
  atom* ptr = allocator_new(&global_allocator);
  ptr->kind = kind;
  ptr->rc = 1;
  alloc_count_per_kind[(int)kind]++;
  return ptr;
}

atom* atom_rc_incr(atom* a) {
  if (a == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  if (a->rc >= SHRT_MAX) {
    fprintf(stderr, "RC_MAX reached. Leaking memory for now.\n");
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
  if (a == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  if (a->rc > 0) a->rc--;
  if (a->rc == 0) {
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
    if (a->kind == MACRO) {
      a->val.as_macro = atom_rc_decr(a->val.as_macro);
    }
    allocator_free(&global_allocator, a);
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
  printf("  live objects: %ld\n", allocator_live_atoms(&global_allocator));
  printf("  slabs: %ld\n", allocator_slabs_count(&global_allocator));
}

void rc_memleak_check(void) {
  size_t allocated_objects = allocator_live_atoms(&global_allocator);

  if (allocated_objects > 0) {
    fprintf(stderr, "Tracked allocated objects: %ld\n", allocated_objects);
    error(ERR_LEAK_CODE, ERR_LEAK_MSG, __func__, __FILE__, __LINE__);
  }
}

