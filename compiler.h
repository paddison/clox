#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "vm.h"

typedef enum {
  TYPE_UPVALUE,
  TYPE_LOCAL,
  TYPE_LOOP_UPVALUE,
  TYPE_LOOP_LOCAL,
} UpvalueType;

ObjFunction *compile(const char *source);

#endif
