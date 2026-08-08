#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "mem.h"

#define RC_DISABLED_DUE_TO_STATIC_ALLOC -1

typedef struct { size_t len; char val[8]; } string_7_t;

extern atom _static_nil;
extern atom _static_true;
extern atom _static_sym_lambda;
extern atom _static_sym_quote;
extern atom _static_sym_let;
extern atom _static_sym_thunk;
extern atom _static_sym_if;
extern atom _static_sym_minus;
extern atom _static_sym_number;

extern atom* initial_global_symbols;
extern atom* global_symbols;

#endif
