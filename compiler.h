#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "vm.h"

typedef enum {
  TypeUpValue,
  TypeLocal,
  TypeLoop,
} UpValueType;

ObjFunction *compile(const char *source);

#endif
