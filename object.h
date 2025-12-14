#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "value.h"

// clang-format off
/*******************************************************************************
 * Encode the mark as a flag in the type header:
 * obj->type:
 * |mttt tttt|
 * |0000_0000|
 * the 't' bits encode the type, whereas the 'm' bit encodes if the mark is set.
 *******************************************************************************/
#define OBJ_TYPE(value)         (AS_OBJ(value)->type & 0x7F) // extract the 7 lsb
#define TYPE(obj)               (obj->type & 0x7F)           // extract the 7 lsb
#define IS_MARKED(obj)          ((obj)->type & 0x80)           // check the 'm' bit
#define MARK(obj)               (obj->type |= 0x80)          // set the 'm' bit
#define FREE_MARK(obj)          (obj->type &= 0x7F)          // unset the 'm' bit

#define IS_CLOSURE(value)       (isObjType(value, OBJ_CLOSURE))
#define IS_FUNCTION(value)      (isObjType(value, OBJ_FUNCTION))
#define IS_STRING(value)        (isObjType(value, OBJ_STRING) || isObjType(value, OBJ_CONST_STRING))
#define IS_ARRAY(value)         (isObjType(value, OBJ_ARRAY))

#define AS_CLOSURE(value)       ((ObjClosure*)    AS_OBJ(value))
#define AS_FUNCTION(value)      ((ObjFunction*)   AS_OBJ(value))
#define AS_NATIVE(value)        (((ObjNative*)    AS_OBJ(value)))
#define AS_STRING(value)        ((ObjString*)     AS_OBJ(value))
#define AS_CONST_STRING(value)  ((ObjConstString*)AS_OBJ(value))
#define AS_CSTRING(value)       (((ObjString*)    AS_OBJ(value))->chars)
#define AS_ARRAY(value)         ((ObjArray*)      AS_OBJ(value))
// clang-format on

typedef enum {
  OBJ_FUNCTION = 0,
  OBJ_NATIVE = 1,
  OBJ_STRING = 2,
  OBJ_CONST_STRING = 3,
  OBJ_ARRAY = 4,
  OBJ_CLOSURE = 5,
  OBJ_UPVALUE = 6,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj *next;
};

typedef struct {
  Obj obj;
  int arity;
  Chunk chunk;
  ObjString *name;
  int upvalueCount;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value *args);

typedef struct {
  NativeFn fn;
  char *name;
  int arity;
} Native;

typedef struct {
  Obj obj;
  NativeFn function;
  int arity;
} ObjNative;

struct ObjString {
  Obj obj;
  int length;
  uint32_t hash;
  char chars[];
};

typedef struct ObjUpvalue {
  Obj obj;
  Value *location;
  Value closed;
  struct ObjUpvalue *next;
} ObjUpvalue;

struct ObjConstString {
  Obj obj;
  int length;
  const char *chars;
};

typedef struct {
  Obj obj;
  ObjFunction *function;
  ObjUpvalue **upvalues;
  int upvalueCount;
} ObjClosure;

struct ObjArray {
  Obj obj;
  ValueArray array;
};

ObjClosure *newClosure(ObjFunction *function);
ObjFunction *newFunction();
ObjNative *newNative(NativeFn function, int arity);
ObjString *allocateEmptyString(const int length);
ObjString *takeString(char *chars, int length);
ObjString *copyString(const char *chars, int length);
ObjUpvalue *newUpvalue(Value *slot);
ObjConstString *constString(const char *chars, int length);
ObjArray *allocateEmptyArray();
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
