#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"

/*
 * MAIN
 */
int main(void) {
  printf("lispw by Quentin Dufour <quentin@dufour.io>\n");
  atom* my_env = full_env();
  while (true) {
    printf("> ");
    atom* my_tokens = lex(stdin);
    if (my_tokens->kind == NIL) break;
    atom* my_parsing = expr(my_tokens);
    atom* my_ast = car(my_parsing);
    atom* my_eval = eval(my_ast, my_env);
    atom* my_eval_forced = force_it(my_eval);
    atom* my_sexpr = sexpr(my_eval_forced);
    print(my_sexpr);
    atom_rc_decr(my_sexpr);
    atom_rc_decr(my_eval_forced);
    atom_rc_decr(my_eval);
    atom_rc_decr(my_ast);
    atom_rc_decr(my_parsing);
    atom_rc_decr(my_tokens);
  }
  atom_rc_decr(my_env);

  store_free();
  symbols_free();
  rc_memleak_check();

  return 0;
}
