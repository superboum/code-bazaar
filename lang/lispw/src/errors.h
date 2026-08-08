#ifndef ERRORS_H
#define ERRORS_H

#define ERR_MALLOC_CODE 100
#define ERR_MALLOC_MSG "Not enough memory."
#define ERR_LOGIC_CODE 101
#define ERR_LOGIC_MSG "Internal logic error."
#define ERR_ATOM_WRONG_TYPE_CODE 102
#define ERR_ATOM_WRONG_TYPE_MSG "An atom of a given type was expected; something else was found."
#define ERR_CANT_CAR_CODE 103
#define ERR_CANT_CAR_MSG "can't car or cdr this atom as it's not a pair."
#define ERR_RC_ERROR_CODE 104
#define ERR_RC_ERROR_MSG "reference counting logic error; object is freed."
#define ERR_PARSER_ERROR_CODE 105
#define ERR_PARSER_ERROR_MSG "Parser failed. You probably have a syntax error in your code"
#define ERR_INTERPRETER_CODE 106
#define ERR_INTERPRETER_MSG "Interpreter failed. Check your syntax."
#define ERR_LEAK_CODE 107
#define ERR_LEAK_MSG "Memory leak detected: some allocated objects were not deallocated."
#define ERR_UNDEFINED_CODE 108
#define ERR_UNDEFINED_MSG "Tried to resolve a variable that does not exist."
#define ERR_SLAB_CODE 109
#define ERR_SLAB_MSG "An internal error occured in the slab memory allocator"

void error(int code, char* msg, const char* fn, const char* file, int line);

#endif
