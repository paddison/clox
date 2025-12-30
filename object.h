#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

// clang-format off
#define OBJ_TYPE(value)         (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value)  (isObjType(value, OBJ_BOUND_METHOD))
#define IS_CLASS(value)         (isObjType(value, OBJ_CLASS))
#define IS_INSTANCE(value)      (isObjType(value, OBJ_INSTANCE))
#define IS_CLOSURE(value)       (isObjType(value, OBJ_CLOSURE))
#define IS_FUNCTION(value)      (isObjType(value, OBJ_FUNCTION))
#define IS_STRING(value)        (isObjType(value, OBJ_STRING) || isObjType(value, OBJ_CONST_STRING))
#define IS_CONST_STRING(value)  (isObjType(value, OBJ_CONST_STRING))
#define IS_ARRAY(value)         (isObjType(value, OBJ_ARRAY))

#define AS_BOUND_METHOD(value)  ((ObjBoundMethod*)AS_OBJ(value))
#define AS_CLASS(value)         ((ObjClass*)      AS_OBJ(value))
#define AS_INSTANCE(value)      ((ObjInstance*)   AS_OBJ(value))
#define AS_CLOSURE(value)       ((ObjClosure*)    AS_OBJ(value))
#define AS_FUNCTION(value)      ((ObjFunction*)   AS_OBJ(value))
#define AS_NATIVE(value)        ((ObjNative*)     AS_OBJ(value))
#define AS_STRING(value)        ((ObjString*)     AS_OBJ(value))
#define AS_CONST_STRING(value)  ((ObjConstString*)AS_OBJ(value))
#define AS_CSTRING(value)       (((ObjString*)    AS_OBJ(value))->chars)
#define AS_ARRAY(value)         ((ObjArray*)      AS_OBJ(value))
// clang-format on

typedef enum {
  OBJ_BOUND_METHOD,
  OBJ_CLASS,
  OBJ_FUNCTION,
  OBJ_INSTANCE,
  OBJ_NATIVE,
  OBJ_STRING,
  OBJ_CONST_STRING,
  OBJ_ARRAY,
  OBJ_CLOSURE,
  OBJ_UPVALUE,
} ObjType;

struct Obj {
  ObjType type;
  bool isMarked;
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
  uint32_t hash;
  const char *chars;
};

typedef struct {
  Obj obj;
  ObjFunction *function;
  ObjUpvalue **upvalues;
  int upvalueCount;
} ObjClosure;

typedef struct ObjClass {
  Obj obj;
  struct ObjClass *superclass;
  ObjString *name;
  Table **methods;
  int hierarchySize;
} ObjClass;

typedef struct {
  Obj obj;
  ObjClass *klass;
  Table fields;
} ObjInstance;

typedef struct {
  Obj obj;
  Value receiver;
  ObjClosure *method;
} ObjBoundMethod;

struct ObjArray {
  Obj obj;
  ValueArray array;
};

ObjBoundMethod *newBoundMethod(Value receiver, ObjClosure *method);
ObjClass *newClass(ObjString *name);
ObjClosure *newClosure(ObjFunction *function);
ObjFunction *newFunction();
ObjInstance *newInstance(ObjClass *klass);
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
