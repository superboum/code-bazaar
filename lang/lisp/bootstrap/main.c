#include <stdlib.h>
#include <stdio.h>

#define MALLOC_ERR 100
#define MALLOC_MSG "Malloc failed"

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
  char* content;
} string_t;

string_t* string(char* s, int len) {
  char* buf = malloc(sizeof(char)*len);
  if (buf == NULL) error(MALLOC_ERR, MALLOC_MSG);
  string_t* final = malloc(sizeof(string_t));
  if (final == NULL) error(MALLOC_ERR, MALLOC_MSG);
  final->content = buf;
  final->len = len;
  for (size_t i = 0; i < len; i++) {
    final->content[i] = s[i];
  }
  return final;
}

void string_free(string_t* ptr) {
  free(ptr->content);
  free(ptr);
}

// -- atoms
#define ATOM_SLOTS 4096
#define ATOM_MAX_LEN 16
typedef struct atom_slot {
  short next;
  short down;
  char letter;
} atom_elem;
typedef struct atom_trie {
  atom_elem trie[ATOM_SLOTS];
  short cursor;
} atom_trie;
atom_trie* new_atom_trie() {
  atom_trie* ptr = malloc(sizeof(atom_trie));
  if (ptr == NULL) error(MALLOC_ERR, MALLOC_MSG);
  memset(ptr, -1, sizeof(atom_trie));
  return ptr;
}

short atom(atom_trie* at, char* v, int len) {
  if (len > ATOM_MAX_LEN) error(ATOM_LEN_ERR, ATOM_LEN_MSG);
  atom_slot* as = &(struct atom_slot) { .letter='\0', .next=-1, .down=0 };
  short pos = -1;
  for (int i = 0; i < len; i++) {
    // Find down value (create it if needed)
    if (as->down == -1) {
      as->down = at.cursor++;
      pos = as->down
      as = &at.trie[pos];
      as->letter = v[i];
    } else {
      pos = as->down;
      as = &at.trie[pos];
    }

    // Find current letter at this trie level (or create it)
    while (true) {
      if (as->letter == v[i]) break;
      if (as->next == -1) {
        as->next = at.cursor++;
	pos = as->next;
	as = &at.trie[pos];
	as->letter = v[i];
	break;
      }
      pos = as->next;
      as = &at.trie[pos];
    }
  }
  return pos;
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
  printf("hello\n");
  return 0;
}
