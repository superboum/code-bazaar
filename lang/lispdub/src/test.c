#include <stdlib.h>
#include <stdio.h>
#include "runtime.h"

char* hello_world = "hello world";
int ensure(char* candidate, atom* expected);
int main(void) {
  int exit_code = 0;

  exit_code += ensure("./examples/bool_true.lisp", _true());
  exit_code += ensure("./examples/bool_false.lisp", _false());
  exit_code += ensure("./examples/lazy_if.lisp", _true());
  exit_code += ensure("./examples/list.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 4001 });
  exit_code += ensure("./examples/apply.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 5 });
  exit_code += ensure("./examples/number.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 5 });
  exit_code += ensure("./examples/math.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 10 });
  exit_code += ensure("./examples/fact.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 120 });
  exit_code += ensure("./examples/fib.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 377 });
  exit_code += ensure("./examples/collatz.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 111 });
  exit_code += ensure("./examples/euler_01.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 233168 });
  exit_code += ensure("./examples/euler_02.lisp", &(atom) { .kind = NUMBER, .rc = -1, .val.as_number = 3382 });

  if (exit_code > 0) fprintf(stderr, "ERROR. %d failed tests\n", exit_code);
  return exit_code;
}

int ensure(char* candidate, atom* expected) {
  int success = false;

  FILE* f = fopen(candidate, "r");
  if (!f) exit(1);

  atom* local_tokens = lex(f);
  if (local_tokens->kind == NIL) exit(1);
  atom* local_parsing = expr(local_tokens);
  atom* local_ast = car(local_parsing);
  atom* local_env = full_env();
  atom* local_eval = eval(local_ast, local_env);
  atom* local_forced = force_it(local_eval);
  success = boolc(eq(local_forced, expected));
  if (success) {
    fprintf(stderr, "PASSED %s\n", candidate);
  } else {
    fprintf(stderr, "FAILED %s\n", candidate);
    atom* branch_expected = sexpr(expected);
    atom* branch_got = sexpr(local_forced);
    printf("  Expected: (kind: %d, ptr: %p) %s\n", expected->kind, expected, branch_expected->val.as_string->val);
    printf("  Got: (kind: %d, ptr: %p) %s\n", local_forced->kind, local_forced, branch_got->val.as_string->val);
    atom_rc_decr(branch_got);
    atom_rc_decr(branch_expected);
  }
  atom_rc_decr(local_forced);
  atom_rc_decr(local_eval);
  atom_rc_decr(local_env);
  atom_rc_decr(local_ast);
  atom_rc_decr(local_parsing);
  atom_rc_decr(local_tokens);

  symbols_free();
  rc_memleak_check();
  fclose(f);

  return success ? 0 : 1;
}
