#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MALLOC_FAILED 13
#define LOGIC_ERROR 14
#define PARSER_ERROR 15

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

int valid_atom_char(char c) {
  if (c <= 32) return -1;
  if (c >= 127) return -1;
  if (c == ')' || c == '('  || c == '@' || c == '!') return -1;
  return 0;
}

struct symbol_res lex_atom(struct list* atom_env) {
  char stack_buffer[256] = {0};
  struct string atom_str = { .len = 0, .buffer=stack_buffer };

  while(atom_str.len < 256) {
    int candidate = getchar();
    if (candidate < 0 || candidate > 255 || valid_atom_char((char)candidate) != 0) {
      ungetc(candidate, stdin);
      break;
    }
    atom_str.buffer[atom_str.len++] = (char)candidate;
  }

  if (atom_str.len < 1) exit(LOGIC_ERROR);
  return symbol(atom_env, &atom_str);
}

struct lex_res {
  struct list *tokens;
  struct list *atom_env;
};

struct lex_res lex(struct lexer_tokens* tok) {
  struct list* atom_env = NULL;
  struct list* tokens = NULL;

  while (true) {
    int cur = getchar();
    if (cur == EOF || cur > 255 || cur < 0) {
      return (struct lex_res){ 
	.tokens = reverse(tokens), 
	.atom_env = atom_env,
      };
    }
    char safe_cur = (char) cur;
    if (safe_cur == '(') {
      tokens = cons(cons(tok->lparen, NULL), tokens);
    } else if (safe_cur == ')') {
      tokens = cons(cons(tok->rparen, NULL), tokens);
    } else if (safe_cur == '!') {
      tokens = cons(cons(tok->define, NULL), tokens);
    } else if (safe_cur == '@') {
      tokens = cons(cons(tok->lambda, NULL), tokens);
    } else if (valid_atom_char(safe_cur) == 0) {
      ungetc(safe_cur, stdin);
      struct symbol_res atom = lex_atom(atom_env);
      tokens = cons(cons(tok->atom, cons(atom.symbol_ref, NULL)), tokens);
    } else {
      // ignore
    }
  }
}

struct string* token_symbol(struct list* prog_tokens) {
  struct list* token_with_data = prog_tokens->value;
  return token_with_data->value;
}

void lex_print(struct list* prog_tokens) {
  while (prog_tokens != NULL) {
    struct list* token_with_data = prog_tokens->value;
    struct string* token_alone = token_with_data->value;
    str_print(token_alone);
    if(token_with_data->next != NULL) {
      struct list* data_part = token_with_data->next;
      printf("(");
      str_print(data_part->value);
      printf(")");
    }
    printf(" ");
    prog_tokens = prog_tokens->next;
  }
  printf("\n");
}

// --- PARSER

/*
 * expr = LPAREN comp-expr RPAREN | ATOM
 * comp-expr =
 *   DEFINE define-expr | 
 *   LAMBDA lambda-expr |
 *   expr expr
 * define-expr = ATOM expr expr
 * lambda-expr = ATOM expr
 */

struct list* p_expr(struct lexer_tokens* tok, struct list* tokens);

struct list* p_define_expr(struct lexer_tokens* tok, struct list* tokens) {
  if (tokens == NULL) exit(PARSER_ERROR);
  struct string* symb = token_symbol(tokens);
  printf("@define-expr\n");
  lex_print(tokens);

  if (tok->atom != symb) exit(PARSER_ERROR);
  tokens = tokens->next;

  tokens = p_expr(tok, tokens);
  tokens = p_expr(tok, tokens);
  return tokens;
}

struct list* p_lambda_expr(struct lexer_tokens* tok, struct list* tokens) {
  if (tokens == NULL) exit(PARSER_ERROR);
  struct string* symb = token_symbol(tokens);
  printf("@lambda-expr\n");
  lex_print(tokens);

  if (tok->atom != symb) exit(PARSER_ERROR);
  tokens = tokens->next;

  tokens = p_expr(tok, tokens);
  return tokens;
}

struct list* p_comp_expr(struct lexer_tokens* tok, struct list* tokens) {
  if (tokens == NULL) exit(PARSER_ERROR);
  struct string* symb = token_symbol(tokens);
  printf("@comp-expr\n");
  lex_print(tokens);

  if (tok->define == symb) {
    tokens = p_define_expr(tok, tokens->next);
  } else if (tok->lambda == symb) {
    tokens = p_lambda_expr(tok, tokens->next);
  } else {
    tokens = p_expr(tok, tokens);
    tokens = p_expr(tok, tokens);
  }

  return tokens;
}

struct list* p_expr(struct lexer_tokens* tok, struct list* tokens) {
  if (tokens == NULL) exit(PARSER_ERROR);
  struct string* symb = token_symbol(tokens);
  printf("@expr\n");
  lex_print(tokens);

  if (tok->lparen == symb) {
    tokens = p_comp_expr(tok, tokens->next);
    symb = token_symbol(tokens);
    if (tok->rparen != symb) exit(PARSER_ERROR);
    tokens = tokens->next;
  } else if (tok->atom == symb) {
    tokens = tokens->next;
  } else {
    exit(PARSER_ERROR);
  }

  return tokens;
}



int main(void) {
  struct lexer_tokens toks = new_lexer_tokens();
  //symbol_env_print(reverse(toks.env));
  printf("-- lex --\n");
  struct lex_res prog_lex = lex(&toks);
  lex_print(prog_lex.tokens);
  printf("-- parse --\n");
  p_expr(&toks, prog_lex.tokens);

  return 0;
}
