#include "symbols.h"

atom _static_nil = (struct atom) { 
  .kind = NIL, 
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC 
};

static string_7_t _static_true_str = {
  .len = 1,
  .val = "t",
};
atom _static_true = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t *)&_static_true_str
};

static string_7_t _static_str_lambda = {
  .len = 6,
  .val = "lambda",
};
atom _static_sym_lambda = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t*)&_static_str_lambda
};

static string_7_t _static_str_quote = {
  .len = 5,
  .val = "quote",
};
atom _static_sym_quote = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t*)&_static_str_quote
};

static string_7_t _static_str_let = {
  .len = 3,
  .val = "let",
};
atom _static_sym_let = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t*)&_static_str_let
};

static string_7_t _static_str_thunk = {
  .len = 5,
  .val = "thunk",
};
atom _static_sym_thunk = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t*)&_static_str_thunk
};

static string_7_t _static_str_if = {
  .len = 2,
  .val = "if",
};
atom _static_sym_if = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t*)&_static_str_if
};

static string_7_t _static_str_minus = {
  .len = 1,
  .val = "-",
};
atom _static_sym_minus = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t*)&_static_str_minus
};

static string_7_t _static_str_number = {
  .len = 6,
  .val = "number",
};
atom _static_sym_number = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t*)&_static_str_number
};

static string_7_t _static_str_define = {
  .len = 6,
  .val = "define",
};
atom _static_sym_define = (struct atom) {
  .kind = SYMBOL,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_string = (string_t*)&_static_str_define
};


atom global_symbols_p0 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_true,
  .val.as_pair.tail = &_static_nil,
};
atom global_symbols_p1 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_sym_lambda,
  .val.as_pair.tail = &global_symbols_p0,
};
atom global_symbols_p2 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_sym_quote,
  .val.as_pair.tail = &global_symbols_p1,
};
atom global_symbols_p3 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_sym_let,
  .val.as_pair.tail = &global_symbols_p2,
};
atom global_symbols_p4 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_sym_thunk,
  .val.as_pair.tail = &global_symbols_p3,
};
atom global_symbols_p5 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_sym_if,
  .val.as_pair.tail = &global_symbols_p4,
};
atom global_symbols_p6 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_sym_minus,
  .val.as_pair.tail = &global_symbols_p5,
};
atom global_symbols_p7 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_sym_number,
  .val.as_pair.tail = &global_symbols_p6,
};
atom global_symbols_p8 = (struct atom) {
  .kind = PAIR,
  .rc = RC_DISABLED_DUE_TO_STATIC_ALLOC,
  .val.as_pair.head = &_static_sym_define,
  .val.as_pair.tail = &global_symbols_p7,
};
atom* initial_global_symbols = &global_symbols_p8;
atom* global_symbols = &global_symbols_p8;


