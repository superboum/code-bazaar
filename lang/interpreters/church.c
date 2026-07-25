#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MALLOC_FAILED 13

// ---- Generic datastructures
// -- List
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

struct list* reverse(struct list* source) {
  struct list* dst = NULL;
  while (source != NULL) {
    dst = cons(source->value, dst);
    source = source->next;
  }
  return dst;
}

// -- Symbols

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

void symbol_env_print(struct list* symbol_env) {
  while(symbol_env != NULL) {
    str_print(symbol_env->value);
    printf("\n");
    symbol_env = symbol_env->next;
  }
}

// ------ Compilation
// -- Lexer
struct lexer_tokens {
  struct list* env;
  struct string* lparen;
  struct string* rparen;
  struct string* define;
  struct string* lambda;
  struct string* atom;
};

struct lexer_tokens new_lexer_tokens() {
  struct lexer_tokens res = { .env = NULL, .lparen = NULL, .rparen = NULL, .define = NULL, .lambda = NULL, .atom = NULL };

  // Prepare tokens
  struct symbol_res lparen_res = symbol(res.env, &(struct string){ .len=10, .buffer="LEFT_PAREN"});
  res.lparen = lparen_res.symbol_ref;
  res.env = lparen_res.symbol_env;

  struct symbol_res rparen_res = symbol(res.env, &(struct string){ .len=11, .buffer="RIGHT_PAREN"});
  res.rparen = rparen_res.symbol_ref;
  res.env = rparen_res.symbol_env;

  struct symbol_res define_res = symbol(res.env, &(struct string){ .len=6, .buffer="DEFINE"});
  res.define = define_res.symbol_ref;
  res.env = define_res.symbol_env;

  struct symbol_res lambda_res = symbol(res.env, &(struct string){ .len=6, .buffer="LAMBDA"});
  res.lambda = lambda_res.symbol_ref;
  res.env = lambda_res.symbol_env;

  struct symbol_res atom_res = symbol(res.env, &(struct string){ .len=6, .buffer="ATOM"});
  res.atom = atom_res.symbol_ref;
  res.env = atom_res.symbol_env;

  return res;
}

struct list* lex(struct lexer_tokens* tok) {
  struct list* tokens = NULL;

  while (true) {
    int cur = getchar();
    if (cur == EOF || cur > 255 || cur < 0) {
      return reverse(tokens);
    }
    char safe_cur = (char) cur;
    if (safe_cur == '(') {
      tokens = cons(tok->lparen, tokens);
    } else if (safe_cur == ')') {
      tokens = cons(tok->rparen, tokens);
    } else if (safe_cur == '!') {
      tokens = cons(tok->define, tokens);
    } else if (safe_cur == '@') {
      tokens = cons(tok->lambda, tokens);
    } else {
      // do nothing for now...
    }
  }
}

int main(void) {
  struct lexer_tokens toks = new_lexer_tokens();
  //symbol_env_print(reverse(toks.env));
  struct list* prog_tokens = lex(&toks);
  while (prog_tokens != NULL) {
    str_print(prog_tokens->value);
    printf(" ");
    prog_tokens = prog_tokens->next;
  }

  return 0;
}
