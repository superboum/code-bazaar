#include <stdlib.h>
#include <stdio.h>
#include <execinfo.h>
#include "errors.h"

#define BACKTRACE_MAX_SZ 1024
void print_backtrace(void)
{
  void *bt[BACKTRACE_MAX_SZ];
  int bt_size;
  char **bt_syms;

  bt_size = backtrace(bt, 1024);
  bt_syms = backtrace_symbols(bt, bt_size);
  for (int i = 1; i < bt_size; i++) {
    fprintf(stderr, "%s\n", bt_syms[i]);
  }
  free(bt_syms);
}

void error(int code, char* msg, const char* fn, const char* file, int line) {
  //print_backtrace();
  fprintf(stderr, "Fatal Error in func %s at %s, line %d. %s\n", fn, file, line, msg);
  exit(code);
}
