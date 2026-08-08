#include "runtime.h"

/*
 * DATATYPES PRIMITIVES
 */

atom* _nil = &_static_nil;
atom* nil() {
  return _nil;
}

atom* _false() {
  return _nil;
}

atom* _true() {
  return &_static_true;
}

atom* cbool(int b) {
  if(b) return _true();
  return _false();
}
int boolc(atom* a) {
  if (a->kind == NIL) return 0;
  return 1;
}

atom* weak(atom* orig) {
  atom* out_res = atom_alloc(WEAK);
  out_res->val.as_weak = orig;
  return out_res;
}

atom* cons(atom* left, atom* right) {
  // left & right are now owned by the result; so we decrement as we consume, and increment as we bind to the new struct
  if (left == NULL || right == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a = atom_alloc(PAIR);
  a->val.as_pair.head = atom_rc_incr(left);
  a->val.as_pair.tail = atom_rc_incr(right);
  return a;
}

atom* cdr(atom* list) {
  if (list == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* local_list = force_it(list);
  if (local_list->kind != PAIR) 
    error(ERR_CANT_CAR_CODE, ERR_CANT_CAR_MSG, __func__, __FILE__, __LINE__);

  atom* out_res = atom_rc_incr(local_list->val.as_pair.tail);
  atom_rc_decr(local_list);
  return out_res;
}

atom* nth(atom* list, int pos) {
  atom* local_list = force_it(list);
  for (int i = 0; i < pos; i++) {
    if (local_list == NULL) 
      error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
    if (local_list->kind != PAIR) 
      error(ERR_CANT_CAR_CODE, ERR_CANT_CAR_MSG, __func__, __FILE__, __LINE__);
    atom* next_local_list = force_it(local_list->val.as_pair.tail);
    atom_rc_decr(local_list);
    local_list = next_local_list;
  }
  if (local_list == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  if (local_list->kind != PAIR) 
    error(ERR_CANT_CAR_CODE, ERR_CANT_CAR_MSG, __func__, __FILE__, __LINE__);

  atom* out_res = atom_rc_incr(local_list->val.as_pair.head);
  atom_rc_decr(local_list);
  return out_res;
}

atom* car(atom* list) {
  return nth(list, 0);
}

atom* cadr(atom* list) {
  return nth(list, 1);
}

atom* caddr(atom* list) {
  return nth(list, 2);
}

atom* cadddr(atom* list) {
  return nth(list, 3);
}

atom* empty(atom* at) {
  // NO RC with bools
  if (at == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* r = _false();
  atom* local_a = force_it(at);
  if (local_a->kind == NIL) r = _true();
  atom_rc_decr(local_a);
  return r;
}
atom* not(atom* a) {
  // NO RC with bools
  return empty(a);
}

atom* length(atom* list) {
  if (list == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  int len = 0;
  while (boolc(list)) {
    len++;
    list = atom_rc_decr(cdr(list)); // protected by list root
  }

  return cnumber(len);
}

atom* reverse(atom* list) {
  if (list == NULL)
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* acc = nil();
  atom* local_list = force_it(list);
  while (boolc(local_list)) {
    // fetch 1st element
    atom* local_head = car(local_list);

    // update acc
    atom* new_acc = cons(local_head, acc);
    atom_rc_decr(local_head);
    atom_rc_decr(acc); // old accumulator is not required anymore
    acc = new_acc;

    // resolve next with force_it
    atom* local_next = cdr(local_list);
    atom* new_list = force_it(local_next);
    atom_rc_decr(local_list);

    // iter
    local_list = new_list;
    atom_rc_decr(local_next);
  }
  atom_rc_decr(local_list);
  return acc;
}

string_t* heap_string(char* s, size_t len) {
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) 
    error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
  memset(ptr, 0, memsz);
  ptr->len = len;
  strncpy(ptr->val, s, len);
  return ptr;
}

int cstring_eq(string_t* s1, string_t* s2) {
  if (s1->len != s2->len) return 0;
  if (strncmp(s1->val, s2->val, s1->len) != 0) return 0;
  return 1;
}

atom* eq(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);

  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);

  // by default it's false
  atom* res = _false();
  if (a1->kind != a2->kind) res = _false(); // we don't cast transparently, so trap all type differences here...
  else if (a1->kind == NUMBER) res = cbool(a1->val.as_number == a2->val.as_number); // compare values
  else if (a1->kind == SYMBOL) res = cbool(a1 == a2); // compare mem addr as symbols are deduplicated
  else if (a1->kind == PAIR) res = cbool(a1 == a2); // compare mem addr
  else if (a1->kind == STRING) res = cbool(cstring_eq(a1->val.as_string, a2->val.as_string)); // compare with strcnmp
  else if (a1->kind == NIL) res = _true(); // nil is always equal to nil

  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return res;
}

atom* assoc(atom* key, atom* list) {
  atom* out_res;
  if (list->kind == NIL) return nil();
  if (list->kind != PAIR)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  atom* local_head = car(list);
  if (local_head->kind != PAIR) 
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  atom* local_cand_key = car(local_head);
  if (boolc(eq(local_cand_key, key))) {
    out_res = atom_rc_incr(local_head);
  } else {
    atom* branch_rest = cdr(list);
    out_res = assoc(key, branch_rest);
    atom_rc_decr(branch_rest);
  }
  atom_rc_decr(local_cand_key);
  atom_rc_decr(local_head);
  return out_res;
}

static const int small_num_cache_size = 128;
atom small_num_cache[128] = {0};
atom* cnumber(int64_t v) {
  atom* a;
  if (v >= 0 && v < small_num_cache_size) {
    a = &small_num_cache[v];
    if (a->kind != NUMBER) {
      a->kind = NUMBER;
      a->rc = RC_DISABLED_DUE_TO_STATIC_ALLOC;
      a->val.as_number = v;
    }
  } else {
    a = atom_alloc(NUMBER);
    a->val.as_number = v;
  }
  return a;
}

// char->number
atom* number(atom* charlist) {
  if (charlist == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);

  if (charlist->kind != PAIR)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);

  int64_t acc = 0;
  int64_t base10shift = 1;
  while (boolc(charlist)) {
    atom* charcode = atom_rc_decr(car(charlist));
    if (boolc(eq(charcode, &_static_sym_minus))) {
      acc = -acc;
      break;
    }
    if (charcode->val.as_number < ASCII_CODE_ZERO || charcode->val.as_number > ASCII_CODE_NINE) 
      error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
    int val = charcode->val.as_number - ASCII_CODE_ZERO;
    acc += val * base10shift;
    base10shift = base10shift*10;
    charlist = atom_rc_decr(cdr(charlist));
  }
  return cnumber(acc);
}

atom* plus(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER) 
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  atom* out_res = cnumber(a1->val.as_number + a2->val.as_number);
  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* minus(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER) 
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  atom* out_res = cnumber(a1->val.as_number - a2->val.as_number);
  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* mult(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  atom* out_res = cnumber(a1->val.as_number * a2->val.as_number);
  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* divi(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL)
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  atom* out_res = cnumber(a1->val.as_number / a2->val.as_number);
  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* mod(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL)
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  atom* out_res = cnumber(a1->val.as_number % a2->val.as_number);
  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* gt(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL)
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);

  atom* out_res = cbool(a1->val.as_number > a2->val.as_number);

  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* lt(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL)
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);

  atom* out_res = cbool(a1->val.as_number < a2->val.as_number);

  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* ge(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL)
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);

  atom* out_res = cbool(a1->val.as_number >= a2->val.as_number);

  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* le(atom* a1t, atom* a2t) {
  if (a1t == NULL || a2t == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* a1 = force_it(a1t);
  atom* a2 = force_it(a2t);
  if (a1->kind != NUMBER || a2->kind != NUMBER)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);

  atom* out_res = cbool(a1->val.as_number <= a2->val.as_number);

  atom_rc_decr(a1);
  atom_rc_decr(a2);
  return out_res;
}

atom* cstring(char* s, size_t len) {
  atom* a = atom_alloc(STRING);
  a->val.as_string = heap_string(s, len);
  return a;
}

atom* string(atom* charlist) {
  if (charlist == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* alen = length(charlist);
  size_t len = alen->val.as_number;
  alen = atom_rc_decr(alen); // we don't need alen past this point

  // build string
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) 
    error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
  memset(ptr, 0, memsz); // make sure we initialize with zero
  ptr->len = len;
  for (size_t i = 0; i < len; i++) {
    atom* acharcode = atom_rc_decr(car(charlist)); // protected by list root
    if (acharcode->kind != NUMBER) 
      error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
    int charcode = acharcode->val.as_number;
    if (charcode < 0 || charcode > 255) 
      error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
    ptr->val[i] = charcode;
    charlist = atom_rc_decr(cdr(charlist)); // protected by list root
  }

  atom* res = atom_alloc(STRING);
  res->val.as_string = ptr;
  return res;
}

atom* string_concatenate(atom* a1, atom* a2) {
  if (a1 == NULL || a2 == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  if (a1->kind != STRING && a2->kind != STRING)
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);

  // build new string
  size_t len = a1->val.as_string->len + a2->val.as_string->len;
  size_t memsz = sizeof(string_t)+sizeof(char)*(len+1);
  string_t* ptr = malloc(memsz);
  if (ptr == NULL) 
    error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
  memset(ptr, 0, memsz);
  ptr->len = len;
  strncpy(ptr->val, a1->val.as_string->val, a1->val.as_string->len);
  strncpy(ptr->val+a1->val.as_string->len, a2->val.as_string->val, a2->val.as_string->len);

  // build new atom
  atom* a = atom_alloc(STRING);
  a->val.as_string = ptr;
  return a;
}

atom* _unsafe_symbol(char* inner_val, size_t inner_len) {
  atom* out_res = NULL;

  // Try to find symbol
  int found = false;
  atom* iter = atom_rc_incr(global_symbols);
  while(iter->kind != NIL && !found) {
    atom* loop_cur = car(iter);
			    
    if (loop_cur->kind != SYMBOL) 
      error(ERR_LOGIC_CODE, ERR_LOGIC_MSG, __func__, __FILE__, __LINE__); // wrong type
    string_t* cur_str = loop_cur->val.as_string;

    found = cur_str->len == inner_len; // length must match
    if (found) found = strncmp(cur_str->val, inner_val, inner_len) == 0; // chars must match
    if (found) {
      out_res = atom_rc_incr(loop_cur);
    }

    atom_rc_decr(loop_cur);

    // iter
    atom* loop_next = cdr(iter);
    atom_rc_decr(iter);
    iter = loop_next;
  }
  atom_rc_decr(iter);

  // Symbol not found, creating it (and trigger an allocation)
  if (out_res == NULL) {
    atom* new_symbol = atom_alloc(SYMBOL);
    new_symbol->val.as_string = heap_string(inner_val, inner_len);
    atom* old_global_symbols = global_symbols;
    global_symbols = cons(new_symbol, global_symbols);
    atom_rc_decr(old_global_symbols);
    out_res = new_symbol;
  }

  return out_res;
}

atom* symbol(atom* a) {
  // NOTE: do not use Lisp boolean heres as true is defined as a symbol
  if (a == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  if (a->kind == SYMBOL) return a;
  if (a->kind != STRING) 
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  string_t* inner = a->val.as_string;
  char* inner_val = inner->val;
  size_t inner_len = inner->len;
  return _unsafe_symbol(inner_val, inner_len);
}

atom* csymbol(char* s) {
  return _unsafe_symbol(s, strlen(s));
}

void symbols_free() {
  if (global_symbols != NULL) global_symbols = atom_rc_decr(global_symbols);
  /*enable_tracker = 0;
  for (int i = 0; i < 4096; i++) {
    if (tracker[i] != NULL) {
      atom* render = sexpr(tracker[i]);
      print(render);
      atom_rc_decr(render);
    }
  }*/

  if (global_symbols != NULL && global_symbols != initial_global_symbols) exit(513);
  global_symbols = initial_global_symbols;
}

atom* sexpr(atom* a) {
  if (a->kind == NIL) return cstring("NIL", 3);
  if (a->kind == SYMBOL) {
    const char fmt[] = "%s";
    int sz = snprintf(NULL, 0, fmt, a->val.as_string->val)+1;
    char* tmp = malloc(sz);
    if (tmp == NULL) 
      error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
    memset(tmp, 0, sz);
    snprintf(tmp, sz, fmt, a->val.as_string->val);
    atom* final = cstring(tmp, strlen(tmp));
    free(tmp);
    return final;
  }
  if (a->kind == STRING) {
    const char fmt[] = "\"%s\"";
    int sz = snprintf(NULL, 0, fmt, a->val.as_string->val)+1;
    char* tmp = malloc(sz);
    if (tmp == NULL) 
      error(ERR_MALLOC_CODE, ERR_MALLOC_MSG,  __func__, __FILE__, __LINE__);
    memset(tmp, 0, sz);
    snprintf(tmp, sz, fmt, a->val.as_string->val);
    atom* final = cstring(tmp, strlen(tmp));
    free(tmp);
    return final;
  }
  if (a->kind == NUMBER) {
    const char fmt[] = "%ld";
    int sz = snprintf(NULL, 0, fmt, a->val.as_number)+1;
    char* tmp = malloc(sz);
    if (tmp == NULL) 
      error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
    memset(tmp, 0, sz);
    snprintf(tmp, sz, fmt, a->val.as_number);
    atom* final = cstring(tmp, strlen(tmp));
    free(tmp);
    return final;
  }
  if (a->kind == PAIR) {
    size_t allocated = 3; // left paren + right paren + \0
    size_t cursor = 0;
    char* acc = malloc(allocated);
    if (acc == NULL) 
      error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
    memset(acc, 0, allocated);
    snprintf(acc+cursor, allocated, "(");
    cursor += 1;

    atom* iter = a;
    const char fmt[] = "%s ";
    while (iter->kind == PAIR) {
      atom* head = sexpr(iter->val.as_pair.head);
      size_t add_sz = snprintf(NULL, 0, fmt, head->val.as_string->val);
      allocated += add_sz;
      acc = realloc(acc, allocated);
      if (acc == NULL) 
        error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
      memset(acc+cursor, 0, allocated-cursor);
      snprintf(acc+cursor, allocated-cursor, fmt, head->val.as_string->val);
      cursor += add_sz;
      atom_rc_decr(head);
      iter = iter->val.as_pair.tail;
    }
    if (iter->kind == NIL) {
      cursor--;
    }
    //@FIXME: We must handle the case where tail is not NIL
    // BUT we may need first to add support for dot notations: `(foo . bar)`
    snprintf(acc+cursor, allocated-cursor, ")");

    atom* final = cstring(acc, strlen(acc));
    free(acc);
    return final;
  }
  if (a->kind == FX1 || a->kind == FX2) {
    return cstring("C FUNCTION", 10);
  }
  if (a->kind == CLOSU) {
    return cstring("CLOSURE", 7);
  }
  if (a->kind == THUNK) {
    return cstring("THUNK", 5);
  }
  if (a->kind == WEAK) {
    return cstring("WEAK", 4);
  }

  error(ERR_LOGIC_CODE, ERR_LOGIC_MSG, __func__, __FILE__, __LINE__);
  return NULL; // unreachable
}

atom* debug_sexpr(atom* a) {
  if (a->kind == NIL) return cstring("NIL", 3);
  if (a->kind == SYMBOL) {
    const char fmt[] = "{%d}%s";
    int sz = snprintf(NULL, 0, fmt, a->rc, a->val.as_string->val)+1;
    char* tmp = malloc(sz);
    if (tmp == NULL) 
      error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
    memset(tmp, 0, sz);
    snprintf(tmp, sz, fmt, a->rc, a->val.as_string->val);
    atom* final = cstring(tmp, strlen(tmp));
    free(tmp);
    return final;
  }
  if (a->kind == STRING) {
    const char fmt[] = "{%d}\"%s\"";
    int sz = snprintf(NULL, 0, fmt, a->rc, a->val.as_string->val)+1;
    char* tmp = malloc(sz);
    if (tmp == NULL) 
      error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
    memset(tmp, 0, sz);
    snprintf(tmp, sz, fmt, a->rc, a->val.as_string->val);
    atom* final = cstring(tmp, strlen(tmp));
    free(tmp);
    return final;
  }
  if (a->kind == NUMBER) {
    const char fmt[] = "{%d}%ld";
    int sz = snprintf(NULL, 0, fmt, a->rc, a->val.as_number)+1;
    char* tmp = malloc(sz);
    if (tmp == NULL) 
      error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
    memset(tmp, 0, sz);
    snprintf(tmp, sz, fmt, a->rc, a->val.as_number);
    atom* final = cstring(tmp, strlen(tmp));
    free(tmp);
    return final;
  }
  if (a->kind == PAIR) {
    const char fmt[] = "{%d}(%s %s) ";
    atom* left = debug_sexpr(a->val.as_pair.head);
    atom* right = debug_sexpr(a->val.as_pair.tail);
    int sz = snprintf(NULL, 0, fmt, a->rc, left->val.as_string->val, right->val.as_string->val)+1;
    char* tmp = malloc(sz);
    if (tmp == NULL) 
      error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
    memset(tmp, 0, sz);
    snprintf(tmp, sz, fmt, a->rc, left->val.as_string->val, right->val.as_string->val);
    atom* final = cstring(tmp, strlen(tmp));
    free(tmp);
    atom_rc_decr(left);
    atom_rc_decr(right);
    return final;
  }

  return a;
}

void print(atom* a) {
  if (a->kind != STRING && a->kind != SYMBOL) 
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  printf("%s\n", a->val.as_string->val);
}


/*
 * LEXER
 *
 * We handwrite the lexer but in the spirit we use a Deterministic Finite Automation
 */
int is_digit(char c) {
  return (c >= ASCII_CODE_ZERO && c <= ASCII_CODE_NINE);
}

int is_symbol_char(char c) {
  return (
    c >= ASCII_CODE_EXCLAMATION 
      && c <= ASCII_CODE_TILDE
      && c != ')'
      && c != '('
  );
}

// Returns a (string "foobar")
atom* lex_string(FILE* f) {
  atom* acc = nil();
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || c == '"') break;
    atom* anumber = cnumber(c);
    atom* new_acc = cons(anumber, acc);
    atom_rc_decr(anumber);
    atom_rc_decr(acc);
    acc = new_acc;
  }
  atom* rev = reverse(acc);
  atom* value = string(rev);
  atom* type = csymbol("string");
  atom* final = cons(type, value);
  atom_rc_decr(acc);
  atom_rc_decr(rev);
  atom_rc_decr(type);
  atom_rc_decr(value);
  return final;
}

// Returns a (symbol foo)
atom* lex_symbol(FILE* f) {
  atom* acc = nil();
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || !(is_symbol_char(c))) {
      ungetc(c, f);
      break;
    }
    atom* anumber = cnumber(c);
    atom* new_acc = cons(anumber, acc);
    atom_rc_decr(anumber);
    atom_rc_decr(acc);
    acc = new_acc;
  }
  atom* rev = reverse(acc);
  atom* parsed_string = string(rev);
  atom* final_symbol = symbol(parsed_string);
  atom* type = csymbol("symbol");
  atom* final = cons(type, final_symbol);
  atom_rc_decr(acc);
  atom_rc_decr(rev);
  atom_rc_decr(parsed_string);
  atom_rc_decr(final_symbol);
  atom_rc_decr(type);
  return final;
}

// Returns a (number 67)
// @FIXME missing support for minus
atom* lex_number(FILE* f) {
  atom* acc = nil();

  // handle sign
  int s = fgetc(f);
  if (s == '-') acc = cons(&_static_sym_minus, acc);
  else ungetc(s, f);

  // capture numbers
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF || !(is_digit(c))) {
      ungetc(c, f);
      break;
    }
    atom* anumber = cnumber(c);
    atom* new_acc = cons(anumber, acc);
    atom_rc_decr(anumber);
    atom_rc_decr(acc);
    acc = new_acc;
  }
  atom* anum = number(acc);
  atom* final = cons(&_static_sym_number, anum);
  atom_rc_decr(acc);
  atom_rc_decr(anum);
  return final;
}

void lex_comment(FILE* f) {
  int c = fgetc(f);
  while (c != EOF && c != '\n') {
    c = fgetc(f);
  }
}

// Returns a token. (lparen) | (rparen) | (number 67) | (symbol foo) | (string "blabla")
atom* lex_token(FILE* f) {
  // the loop eats spaces & new lines
  while (true) {
    int c = fgetc(f);
    if (c > 255 || c < 0 || c == EOF) return nil();
    if (c == '(') {
	atom* type = csymbol("lparen");
	atom* final = cons(type, nil());
	atom_rc_decr(type);
	return final;
    }
    if (c == ')') {
	atom* type = csymbol("rparen");
	atom* final = cons(type, nil());
	atom_rc_decr(type);
	return final;
    }
    if (c == '"') {
      return lex_string(f);
    }
    if (c == ';') {
      lex_comment(f);
      continue;
    }
    if (is_digit(c) || c == '-') {
      ungetc(c, f);
      return lex_number(f);
    }
    if (is_symbol_char(c)) {
      ungetc(c, f);
      return lex_symbol(f);
    }
  }
}

atom* lex(FILE* f) {
  atom* acc = nil();
  int dangling_paren = 0;
  atom* lparen = csymbol("lparen");
  atom* rparen = csymbol("rparen");
  while (true) {
    atom* tmp = lex_token(f);
    if (!boolc(tmp)) break;
    atom* loop_kind = car(tmp);
    if (boolc(eq(loop_kind, lparen))) {
      dangling_paren++;
    }
    if (boolc(eq(loop_kind, rparen))) {
      dangling_paren--;
    }
    atom* new_acc = cons(tmp, acc);
    atom_rc_decr(tmp);
    atom_rc_decr(acc);
    atom_rc_decr(loop_kind);
    acc = new_acc;
    if (dangling_paren == 0) break;
  }
  atom* res = reverse(acc);
  atom_rc_decr(acc);
  atom_rc_decr(lparen);
  atom_rc_decr(rparen);
  return res;
}

/*
 * PARSER
 *
 * expr:       LPAREN list | patom
 * list:       expr list | RPAREN 
 * patom:      SYMBOL | NUMBER | STRING
 */

atom* patom(atom* lex);
atom* list(atom* lex);
atom* expr(atom* lex);

atom* patom(atom* lex) {
  atom* out_res;
  if (lex == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* local_candidate = car(lex);
  atom* local_next = cdr(lex);
  atom* local_kind = car(local_candidate);
  atom* local_val = cdr(local_candidate);
  atom* local_symbol = csymbol("symbol");
  atom* local_number = csymbol("number");
  atom* local_string = csymbol("string");

  if (!boolc(eq(local_kind, local_symbol)) && !boolc(eq(local_kind, local_number)) && !boolc(eq(local_kind, local_string))) {
     error(ERR_PARSER_ERROR_CODE, ERR_PARSER_ERROR_MSG, __func__, __FILE__, __LINE__);
  }

  // @FIXME handle NIL() specific case...

  out_res = cons(local_val, local_next);

  atom_rc_decr(local_candidate);
  atom_rc_decr(local_next);
  atom_rc_decr(local_kind);
  atom_rc_decr(local_val);
  atom_rc_decr(local_symbol);
  atom_rc_decr(local_number);
  atom_rc_decr(local_string);
  return out_res;
}

// returns cons(AST . TOKENS)
atom* list(atom* lex) {
  atom* out_res;
  if (lex == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  atom* local_candidate = car(lex);
  atom* local_next = cdr(lex);
  atom* local_kind = car(local_candidate);
  atom* local_rparen = csymbol("rparen");
  
  if (boolc(eq(local_rparen, local_kind))) {
    out_res = cons(nil(), local_next);
  } else {
    atom* expr_res = expr(lex);
    atom* expr_ast = car(expr_res);
    atom* expr_next = cdr(expr_res);

    atom* rec_res = list(expr_next);
    atom* rec_ast = car(rec_res);
    atom* rec_next = cdr(rec_res);

    atom* new_ast = cons(expr_ast, rec_ast);
    out_res = cons(new_ast, rec_next);
    
    atom_rc_decr(new_ast);
    atom_rc_decr(rec_next);
    atom_rc_decr(rec_ast);
    atom_rc_decr(rec_res);
    atom_rc_decr(expr_next);
    atom_rc_decr(expr_ast);
    atom_rc_decr(expr_res);
  }
  atom_rc_decr(local_rparen);
  atom_rc_decr(local_kind);
  atom_rc_decr(local_next);
  atom_rc_decr(local_candidate);
  return out_res;
}

// returns cons(AST . TOKENS)
atom* expr(atom* lex) {
  atom* out_res = nil();
  if (lex == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  if (!boolc(lex)) return cons(nil(), nil());
  atom* local_candidate = car(lex);
  atom* local_next = cdr(lex);
  atom* local_kind = car(local_candidate);
  atom* local_lparen = csymbol("lparen");
  if (boolc(eq(local_kind, local_lparen))) {
    // Get references
    out_res = list(local_next);
  } else {
    out_res = patom(lex);
  }
  atom_rc_decr(local_candidate);
  atom_rc_decr(local_next);
  atom_rc_decr(local_kind);
  atom_rc_decr(local_lparen);
  return out_res;
}

/*
 * INTERPRETER
 */
atom* apply(atom* rator, atom* rands);

atom* thunk(atom* expr, atom* env) {
  atom* out = atom_alloc(THUNK);
  out->val.as_capture.expr = atom_rc_incr(expr);
  out->val.as_capture.env = atom_rc_incr(env);
  return out;
}

atom* force_it(atom* maybe_thunk) {
  if (maybe_thunk->kind != THUNK) return atom_rc_incr(maybe_thunk);
  // memoization lookup
  // @FIXME: a proper MEMOIZED_THUNK type would be better...
  if (maybe_thunk->val.as_capture.env == _nil) return atom_rc_incr(maybe_thunk->val.as_capture.expr);

  // resolve
  atom* partial = eval(maybe_thunk->val.as_capture.expr, maybe_thunk->val.as_capture.env);
  atom* finale = force_it(partial);
  atom_rc_decr(partial);

  atom_rc_decr(maybe_thunk->val.as_capture.expr);
  atom_rc_decr(maybe_thunk->val.as_capture.env);
  maybe_thunk->val.as_capture.expr = atom_rc_incr(finale);
  maybe_thunk->val.as_capture.env = _nil;
  return finale;
}

atom* apply(atom* rator, atom* rands) {
  atom* out_res = nil();
  if (rator == NULL) 
    error(ERR_RC_ERROR_CODE, ERR_RC_ERROR_MSG, __func__, __FILE__, __LINE__);
  if (rator->kind == CLOSU) {
    atom* branch_expr = atom_rc_incr(rator->val.as_capture.expr);
    atom* branch_env = atom_rc_incr(rator->val.as_capture.env);
    atom* branch_var_names = car(branch_expr);
    atom* branch_body = cadr(branch_expr);

    if (branch_var_names->kind == PAIR && rands->kind == PAIR) {
      atom_rc_incr(rands);
    }
    while (branch_var_names->kind == PAIR && rands->kind == PAIR) {
      atom* loop_branch_env = branch_env;
      atom* loop_var_names = branch_var_names;
      atom* loop_rands = rands;
      atom* loop_cur_name = car(branch_var_names);
      atom* loop_cur_val = car(rands);
      atom* loop_env_entry = cons(loop_cur_name, loop_cur_val);

      branch_env = cons(loop_env_entry, loop_branch_env);
      rands = cdr(loop_rands);
      branch_var_names = cdr(loop_var_names);

      atom_rc_decr(loop_env_entry);
      atom_rc_decr(loop_cur_val);
      atom_rc_decr(loop_cur_name);
      atom_rc_decr(loop_rands);
      atom_rc_decr(loop_var_names);
      atom_rc_decr(loop_branch_env);
    }

    out_res = eval(branch_body, branch_env);

    atom_rc_decr(branch_body);
    atom_rc_decr(branch_var_names);
    atom_rc_decr(branch_expr);
    atom_rc_decr(branch_env);
  } else if (rator->kind == FX1) {
    atom* branch_rand1 = car(rands);
    out_res = rator->val.as_fx1(branch_rand1);
    atom_rc_decr(branch_rand1);
  } else if (rator->kind == FX2) {
    atom* branch_rand1 = car(rands);
    atom* branch_rand2 = cadr(rands);
    out_res = rator->val.as_fx2(branch_rand1, branch_rand2);
    atom_rc_decr(branch_rand2);
    atom_rc_decr(branch_rand1);
  } else if (rator->kind == FX3) {
    atom* branch_rand1 = car(rands);
    atom* branch_rand2 = cadr(rands);
    atom* branch_rand3 = caddr(rands);
    out_res = rator->val.as_fx3(branch_rand1, branch_rand2, branch_rand3);
    atom_rc_decr(branch_rand3);
    atom_rc_decr(branch_rand2);
    atom_rc_decr(branch_rand1);
  } else {
    error(ERR_ATOM_WRONG_TYPE_CODE, ERR_ATOM_WRONG_TYPE_MSG, __func__, __FILE__, __LINE__);
  }
  return out_res;
}

atom* eval(atom* ast, atom* env) {
  atom* out_res = nil();

  // handle symbol
  if (ast->kind == SYMBOL) {
    // find symbol in env
    atom* branch_res = assoc(ast, env); 
    if (branch_res->kind != PAIR) {
	fprintf(stderr, "'%s' is not defined.\n", ast->val.as_string->val);
	error(ERR_UNDEFINED_CODE,ERR_UNDEFINED_MSG, __func__, __FILE__, __LINE__);
    }
    atom* branch_expr = cdr(branch_res);
    out_res = eval(branch_expr, env);
    atom_rc_decr(branch_expr);
    atom_rc_decr(branch_res);
  } else if (ast->kind == WEAK) {
    out_res = atom_rc_incr(ast->val.as_weak);
  } else if (ast->kind == PAIR) {
    atom* local_head = car(ast);

    if (boolc(eq(local_head, &_static_sym_lambda))) {
      // (lambda var-list body) -> (closure var-list body env)
      // build a closure
      atom* closu = atom_alloc(CLOSU);
      closu->val.as_capture.expr = cdr(ast);
      closu->val.as_capture.env = atom_rc_incr(env);
      out_res = closu;
    } else if (boolc(eq(local_head, &_static_sym_thunk))) {
      atom* branch_thunk_body = cadr(ast);
      out_res = thunk(branch_thunk_body, env);
      atom_rc_decr(branch_thunk_body);
    } else if (boolc(eq(local_head, &_static_sym_if))) {
      atom* branch_cond = cadr(ast);
      atom* branch_cond_evaled = eval(branch_cond, env);
      atom* branch_cond_resolved = force_it(branch_cond_evaled);
      if (boolc(branch_cond_resolved)) {
        atom* branch_ok = caddr(ast);
	out_res = eval(branch_ok, env);
	atom_rc_decr(branch_ok);
      } else {
	atom* branch_nok = cadddr(ast);
	out_res = eval(branch_nok, env);
	atom_rc_decr(branch_nok);
      }
      atom_rc_decr(branch_cond_resolved);
      atom_rc_decr(branch_cond_evaled);
      atom_rc_decr(branch_cond);
    } else if (boolc(eq(local_head, &_static_sym_quote))) {
      out_res = cadr(ast);
    } else if (boolc(eq(local_head, &_static_sym_let))) {
      // (let (symbol expr) expr)
      atom* local_binding = cadr(ast);
      atom* local_body = caddr(ast);
      atom* local_binding_name = car(local_binding);
      atom* local_binding_expr = cadr(local_binding);

      atom* local_evaled_expr_weak = atom_alloc(WEAK);
      local_evaled_expr_weak->val.as_weak = NULL;
      atom* local_new_env_entry = cons(local_binding_name, local_evaled_expr_weak);
      atom* local_new_env = cons(local_new_env_entry, env);
      atom* local_evaled_expr = thunk(local_binding_expr, local_new_env);
      local_evaled_expr_weak->val.as_weak = local_evaled_expr;

      // eval final body
      out_res = eval(local_body, local_new_env); // eval let body

      atom_rc_decr(local_evaled_expr_weak);
      atom_rc_decr(local_evaled_expr);
      atom_rc_decr(local_new_env);
      atom_rc_decr(local_new_env_entry);
      atom_rc_decr(local_binding_expr);
      atom_rc_decr(local_binding_name);
      atom_rc_decr(local_body);
      atom_rc_decr(local_binding);
    } else {
      // operator operand*
      atom* local_evaled_rator_with_thunk = eval(local_head, env);
      atom* local_evaled_rator = force_it(local_evaled_rator_with_thunk);

      // must be a atom(list(symb(closure))) or a atom(fx1) or a atom(fx2)
      atom* local_evaled_rands = nil();
      atom* local_rands = cdr(ast);
      while (local_rands->kind == PAIR) {
	atom* loop_local_rands = local_rands;
	atom* loop_cur = car(loop_local_rands);
        atom* loop_evaled_cur = thunk(loop_cur, env);
	atom* loop_prev_evaled_rands = local_evaled_rands;

	local_evaled_rands = cons(loop_evaled_cur, loop_prev_evaled_rands);
        local_rands = cdr(loop_local_rands);

	atom_rc_decr(loop_prev_evaled_rands);
	atom_rc_decr(loop_evaled_cur);
        atom_rc_decr(loop_cur);
	atom_rc_decr(loop_local_rands);
      }

      atom* local_rev_evaled_rands = reverse(local_evaled_rands);
      out_res = apply(local_evaled_rator, local_rev_evaled_rands);
      
      atom_rc_decr(local_rev_evaled_rands);
      atom_rc_decr(local_rands);
      atom_rc_decr(local_evaled_rands);
      atom_rc_decr(local_evaled_rator);
      atom_rc_decr(local_evaled_rator_with_thunk);
    }

    atom_rc_decr(local_head);
  } else {
    out_res = atom_rc_incr(ast);
  }

  return out_res;
}

atom* afx1(char* name, fx1 f) {
  atom* out_res;

  atom* local_fx1 = atom_alloc(FX1);
  local_fx1->val.as_fx1 = f;
  atom* local_name = csymbol(name);

  out_res = cons(local_name, local_fx1);

  atom_rc_decr(local_fx1);
  atom_rc_decr(local_name);
  return out_res;
}

atom* afx2(char* name, fx2 f) {
  atom* out_res;

  atom* local_fx2 = atom_alloc(FX2);
  local_fx2->val.as_fx2 = f;
  atom* local_name = csymbol(name);

  out_res = cons(local_name, local_fx2);

  atom_rc_decr(local_fx2);
  atom_rc_decr(local_name);
  return out_res;
}

atom* afx3(char* name, fx3 f) {
  atom* out_res;

  atom* local_fx3 = atom_alloc(FX3);
  local_fx3->val.as_fx3 = f;
  atom* local_name = csymbol(name);

  out_res = cons(local_name, local_fx3);

  atom_rc_decr(local_fx3);
  atom_rc_decr(local_name);
  return out_res;
}

atom* lisp_proc(char* name, char* proc) {
  atom* out_res;

  // build the temporary file
  FILE* f = tmpfile();
  if (!f) error(ERR_MALLOC_CODE, ERR_MALLOC_MSG, __func__, __FILE__, __LINE__);
  fprintf(f, "%s", proc);
  rewind(f);

  atom* local_tokens = lex(f);
  if (local_tokens->kind == NIL) 
    error(ERR_LOGIC_CODE, ERR_LOGIC_MSG, __func__, __FILE__, __LINE__);
  atom* local_parsing = expr(local_tokens);
  atom* local_ast = car(local_parsing);

  atom* local_name = csymbol(name);

  out_res = cons(local_name, local_ast);

  atom_rc_decr(local_name);
  atom_rc_decr(local_ast);
  atom_rc_decr(local_parsing);
  atom_rc_decr(local_tokens);
  fclose(f);

  return out_res;
}

atom* full_env() {
  atom* out_res = nil();
  atom* tmp;
  atom* head;

  atom* name = csymbol("nil");
  head = cons(name, nil());
  tmp = cons(head, out_res);
  atom_rc_decr(head);
  atom_rc_decr(out_res);
  atom_rc_decr(name);
  out_res=tmp;

  head = afx2("cons", cons);
  tmp = cons(head, out_res);
  atom_rc_decr(head);
  atom_rc_decr(out_res);
  out_res=tmp;

  head = afx1("reverse", reverse);
  tmp = cons(head, out_res);
  atom_rc_decr(head);
  atom_rc_decr(out_res);
  out_res=tmp;

  head = afx2("+", plus);
  tmp = cons(head, out_res);
  atom_rc_decr(head);
  atom_rc_decr(out_res);
  out_res=tmp;

  head = afx2("-", minus);
  tmp = cons(head, out_res);
  atom_rc_decr(head);
  atom_rc_decr(out_res);
  out_res=tmp;

  head = afx2("*", mult);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx2("/", divi);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx2("mod", mod);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx2(">", gt);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx2(">=", ge);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx2("<", lt);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx2("<=", le);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx2("eq", eq);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx1("car", car);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx1("cdr", cdr);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx1("cadr", cadr);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx1("caddr", caddr);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = afx1("cadddr", cadddr);
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = lisp_proc("t", "(quote t)");
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = lisp_proc("and", "(lambda (x y) (if x y nil))");
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  head = lisp_proc("or", "(lambda (x y) (if x t y))");
  tmp = cons(head, out_res);
  atom_rc_decr(out_res);
  atom_rc_decr(head);
  out_res=tmp;

  return out_res;
}
