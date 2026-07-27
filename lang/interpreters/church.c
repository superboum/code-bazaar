#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MALLOC_FAILED 13
#define LOGIC_ERROR 14
#define PARSER_ERROR 15
#define AST_ERROR 16
#define INTERPRETER_ERROR 17

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
struct lexer_symbols {
  struct list* env;
  struct string* lparen;
  struct string* rparen;
  struct string* let;
  struct string* lambda;
  struct string* atom;
};

struct lexer_symbols new_lexer_symbols() {
  struct lexer_symbols res = { .env = NULL, .lparen = NULL, .rparen = NULL, .let = NULL, .lambda = NULL, .atom = NULL };

  // Prepare tokens
  struct symbol_res lparen_res = symbol(res.env, &(struct string){ .len=10, .buffer="LEFT_PAREN"});
  res.lparen = lparen_res.symbol_ref;
  res.env = lparen_res.symbol_env;

  struct symbol_res rparen_res = symbol(res.env, &(struct string){ .len=11, .buffer="RIGHT_PAREN"});
  res.rparen = rparen_res.symbol_ref;
  res.env = rparen_res.symbol_env;

  struct symbol_res let_res = symbol(res.env, &(struct string){ .len=3, .buffer="LET"});
  res.let = let_res.symbol_ref;
  res.env = let_res.symbol_env;

  struct symbol_res lambda_res = symbol(res.env, &(struct string){ .len=6, .buffer="LAMBDA"});
  res.lambda = lambda_res.symbol_ref;
  res.env = lambda_res.symbol_env;

  struct symbol_res atom_res = symbol(res.env, &(struct string){ .len=5, .buffer="ATOM"});
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

struct lex_res lex(struct lexer_symbols* tok) {
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
      tokens = cons(cons(tok->let, NULL), tokens);
    } else if (safe_cur == '@') {
      tokens = cons(cons(tok->lambda, NULL), tokens);
    } else if (valid_atom_char(safe_cur) == 0) {
      ungetc(safe_cur, stdin);
      struct symbol_res atom = lex_atom(atom_env);
      atom_env = atom.symbol_env;
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

struct string* token_attached_data(struct list* prog_tokens) {
  struct list* token_with_data = prog_tokens->value;
  struct list* data_part = token_with_data->next;
  return data_part->value;
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
 * expr = LPAREN comp-expr RPAREN | atom
 * comp-expr =
 *   LET let-expr | 
 *   LAMBDA lambda-expr |
 *   expr expr
 * let-expr = atom expr expr
 * lambda-expr = atom expr
 * atom = ATOM
 */

struct parser_symbols {
  struct list* env;
  struct string* let;
  struct string* lambda;
  struct string* apply;
  struct string* atom;
  struct string* clo;
};

struct parser_symbols new_parser_symbols() {
  struct parser_symbols res = { .env = NULL, .let = NULL, .lambda = NULL, .apply = NULL, .atom = NULL, .clo=NULL };

  struct symbol_res let_res = symbol(res.env, &(struct string){ .len=3, .buffer="LET"});
  res.let = let_res.symbol_ref;
  res.env = let_res.symbol_env;

  struct symbol_res lambda_res = symbol(res.env, &(struct string){ .len=6, .buffer="LAMBDA"});
  res.lambda = lambda_res.symbol_ref;
  res.env = lambda_res.symbol_env;

  struct symbol_res apply_res = symbol(res.env, &(struct string){ .len=5, .buffer="APPLY"});
  res.apply = apply_res.symbol_ref;
  res.env = apply_res.symbol_env;

  struct symbol_res atom_res = symbol(res.env, &(struct string){ .len=4, .buffer="ATOM"});
  res.atom = atom_res.symbol_ref;
  res.env = atom_res.symbol_env;

  struct symbol_res clo_res = symbol(res.env, &(struct string){ .len=3, .buffer="CLO"});
  res.clo = clo_res.symbol_ref;
  res.env = clo_res.symbol_env;

  return res;
}

struct p_acc {
  struct list* ast;
  struct list* rem;
  struct lexer_symbols* tok;
  struct parser_symbols* psym;
};

struct p_acc p_expr(struct p_acc acc);

struct p_acc p_atom(struct p_acc acc) {
  if (acc.rem == NULL) exit(PARSER_ERROR);

  struct string* lex_symb = token_symbol(acc.rem);
  if (acc.tok->atom != lex_symb) exit(PARSER_ERROR);
  struct string* parse_symb = token_attached_data(acc.rem);

  acc.ast = cons(acc.psym->atom, cons(parse_symb, NULL));
  acc.rem = acc.rem->next;

  return acc;
}

struct p_acc p_let_expr(struct p_acc acc) {
  if (acc.rem == NULL) exit(PARSER_ERROR);

  // atom
  acc = p_atom(acc);
  struct list* let_name = acc.ast;

  // let binding
  acc = p_expr(acc);
  struct list* let_binding_ast = acc.ast;

  // let body
  acc = p_expr(acc);
  struct list* body_ast = acc.ast;

  // rebuild AST
  acc.ast = cons(acc.psym->let, cons(let_name, cons(let_binding_ast, cons(body_ast, NULL))));

  return acc;
}

struct p_acc p_lambda_expr(struct p_acc acc) {
  if (acc.rem == NULL) exit(PARSER_ERROR);

  // lambda var
  acc = p_atom(acc);
  struct list* lambda_var = acc.ast;

  // lambda body
  acc = p_expr(acc);
  struct list* body_ast = acc.ast;

  // rebuild AST
  acc.ast = cons(acc.psym->lambda, cons(lambda_var, cons(body_ast, NULL)));

  return acc;
}

struct p_acc p_comp_expr(struct p_acc acc) {
  if (acc.rem == NULL) exit(PARSER_ERROR);
  struct string* symb = token_symbol(acc.rem);

  if (acc.tok->let == symb) {
    acc.rem = acc.rem->next;
    acc = p_let_expr(acc);
  } else if (acc.tok->lambda == symb) {
    acc.rem = acc.rem->next;
    acc = p_lambda_expr(acc);
  } else {
    acc = p_expr(acc);
    struct list* operator = acc.ast;
    acc = p_expr(acc);
    struct list* operand = acc.ast;
    acc.ast = cons(acc.psym->apply, cons(operator, cons(operand, NULL)));
  }

  return acc;
}

struct p_acc p_expr(struct p_acc acc) {
  if (acc.rem == NULL) exit(PARSER_ERROR);
  struct string* symb = token_symbol(acc.rem);

  if (acc.tok->lparen == symb) {
    acc.rem = acc.rem->next;
    acc = p_comp_expr(acc);
    if (acc.rem == NULL) exit(PARSER_ERROR);
    symb = token_symbol(acc.rem);
    if (acc.tok->rparen != symb) exit(PARSER_ERROR);
    acc.rem = acc.rem->next;
  } else if (acc.tok->atom == symb) {
    acc = p_atom(acc);
  } else {
    exit(PARSER_ERROR);
  }

  return acc;
}

void print_ast(struct parser_symbols* psym, struct list* ast, int depth) {
  if (ast == NULL) exit(AST_ERROR);
  struct string* head = ast->value;

  if (depth > 0) printf("%*c", depth*2, ' ');
  str_print(head);

  if (psym->let == head) {
    printf("\n");
    struct list* binding_name = ast->next;
    print_ast(psym, binding_name->value, depth+1);

    struct list* binding_val = binding_name->next;
    print_ast(psym, binding_val->value, depth+1);

    struct list* body = binding_val->next;
    print_ast(psym, body->value, depth+1);

    if (body->next != NULL) exit(AST_ERROR);
  } else if (psym->lambda == head) {
    printf("\n");
    struct list* var_name = ast->next;
    print_ast(psym, var_name->value, depth+1);

    struct list* lambda_body = var_name->next;
    print_ast(psym, lambda_body->value, depth+1);

    if (lambda_body->next != NULL) exit(AST_ERROR);
  } else if (psym->apply == head) {
    printf("\n");
    struct list* operator = ast->next;
    print_ast(psym, operator->value, depth+1);

    struct list* operand = operator->next;
    print_ast(psym, operand->value, depth+1);

    if (operand->next != NULL) exit(AST_ERROR);
  } else {
    ast = ast->next;
    printf(" ");
    str_print(ast->value);
    printf("\n");

    if (ast->next != NULL) exit(AST_ERROR);
  }
}

// --- Tree-Walk Interpreter
struct list* eval(struct parser_symbols* psym, struct list* ast, struct list* env);

struct list* apply(struct parser_symbols* psym, struct list* operator, struct list* operand) {
  if (operator == NULL) exit(INTERPRETER_ERROR);
  if (operand == NULL) exit(INTERPRETER_ERROR);
  if (head(operator) != psym->clo) exit(INTERPRETER_ERROR);
  if (rest(operator) == NULL) exit(INTERPRETER_ERROR);
  operator = rest(operator);
  struct string* free_var = head(operator);
  if (rest(operator) == NULL) exit(INTERPRETER_ERROR);
  operator = rest(operator);
  struct list* xator_ast = head(operator);
  if (rest(operator) == NULL) exit(INTERPRETER_ERROR);
  operator = rest(operator);
  struct list* xator_env = head(operator);

  struct list* env = cons(cons(free_var, head(operand)), xator_env);
  return eval(psym, xator_ast, env);
}


struct list* eval(struct parser_symbols* psym, struct list* ast, struct list* env) {
  if (ast == NULL) exit(INTERPRETER_ERROR);
  struct string* kind = head(ast);

  if (kind == psym->atom) {
    if (rest(ast) == NULL) exit(INTERPRETER_ERROR);
    struct string* to_resolve = head(rest(ast));
    while (env != NULL) {
      struct list* current_entry = head(env);
      struct string* env_name = head(current_entry);
      struct list* env_ast = head(rest(current_entry));
      if (env_name == to_resolve) {
        return env_ast;
      }
      env = rest(env);
    }
    printf("not found: ");
    str_print(to_resolve);
    printf("\n");
    exit(INTERPRETER_ERROR);
  } else if (kind == psym->let) {
    printf("let\n");
    if (rest(ast) == NULL) exit(INTERPRETER_ERROR);
    ast = rest(ast);
    struct list* let_atom = head(ast);
    if (rest(let_atom) == NULL) exit(INTERPRETER_ERROR);
    struct string* let_var = head(rest(let_atom));

    if (rest(ast) == NULL) exit(INTERPRETER_ERROR);
    ast = rest(ast);
    struct list* internal_ast = head(ast);
    struct list* internal_clo = eval(psym, internal_ast, env);
    env = cons(cons(let_var, cons(internal_clo, NULL)), env);

    if (rest(ast) == NULL) exit(INTERPRETER_ERROR);
    ast = rest(ast);
    struct list* new_ast = head(ast);

    if (rest(ast) != NULL) exit(INTERPRETER_ERROR);
    return eval(psym, new_ast, env);
  } else if (kind == psym->lambda) {
    printf("lambda\n");
    if (rest(ast) == NULL) exit(INTERPRETER_ERROR);
    ast = rest(ast);
    struct list* free_var_atom = head(ast);
    if (rest(free_var_atom) == NULL) exit(INTERPRETER_ERROR);
    struct string* free_var = head(rest(free_var_atom));

    if (rest(ast) == NULL) exit(INTERPRETER_ERROR);
    ast = rest(ast);
    struct list* body_ast = head(ast);

    if (rest(ast) != NULL) exit(INTERPRETER_ERROR);
    return cons(psym->clo, cons(free_var, cons(body_ast, cons(env, NULL))));
  } else if (kind == psym->apply) {
    if (rest(ast) == NULL) exit(INTERPRETER_ERROR);
    ast = rest(ast);
    struct list* operator = head(ast);

    if (rest(ast) == NULL) exit(INTERPRETER_ERROR);
    ast = rest(ast);
    struct list* operand = head(ast);

    return apply(
      psym,
      eval(psym, operator, env),
      eval(psym, operand, env)
    );

  } else {
    printf("unknown AST node: ");
    str_print(kind);
    printf("\n");
    exit(INTERPRETER_ERROR);
  }
}


int main(void) {
  struct lexer_symbols toks = new_lexer_symbols();
  //symbol_env_print(reverse(toks.env));
  printf("-- lex --\n");
  struct lex_res prog_lex = lex(&toks);
  lex_print(prog_lex.tokens);
  printf("-- parse --\n");
  struct parser_symbols psym = new_parser_symbols();
  struct p_acc parse_res = p_expr((struct p_acc){ 
    .tok = &toks, 
    .psym = &psym,
    .rem = prog_lex.tokens, 
    .ast = NULL
  });
  print_ast(&psym, parse_res.ast, 0);
  printf("-- eval --\n");
  eval(&psym, parse_res.ast, NULL);

  return 0;
}
