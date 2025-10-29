#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

// clang-format off
#define OBJ_TYPE(value)      (AS_OBJ(value)->type)

#define IS_STRING(value)     (isObjType(value, OBJ_STRING) || isObjType(value, OBJ_CONST_STRING))
#define IS_ARRAY(value)      (isObjType(value, OBJ_ARRAY))

#define AS_STRING(value)     ((ObjString*)AS_OBJ(value))
#define AS_CONST_STRING(value)     ((ObjConstString*)AS_OBJ(value))
#define AS_CSTRING(value)    (((ObjString*)AS_OBJ(value))->chars)
#define AS_ARRAY(value)      ((ObjArray*)AS_OBJ(value))
// clang-format on

typedef enum {
  OBJ_STRING,
  OBJ_CONST_STRING,
  OBJ_ARRAY,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj *next;
};

struct ObjString {
  Obj obj;
  int length;
  uint32_t hash;
  char chars[];
};

struct ObjConstString {
  Obj obj;
  int length;
  const char *chars;
};

struct ObjArray {
  Obj obj;
  ValueArray array;
};

ObjString *allocateEmptyString(const int length);
ObjString *takeString(char *chars, int length);
ObjString *copyString(const char *chars, int length);
ObjConstString *constString(const char *chars, int length);
ObjArray *allocateEmptyArray();
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
