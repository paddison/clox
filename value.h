#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include <stdint.h>

typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef struct ObjConstString ObjConstString;
typedef struct ObjArray ObjArray;

typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_NUMBER,
  VAL_INTERNAL,
  VAL_OBJ,
  VAL_ERR,
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
    InternalNum internal;
    Obj *obj;
    const char* err;
  } as;
} Value;

// clang-format off
#define IS_BOOL(value)       ((value).type == VAL_BOOL)
#define IS_NIL(value)        ((value).type == VAL_NIL)
#define IS_NUMBER(value)     ((value).type == VAL_NUMBER)
#define IS_INTERNAL(value)   ((value).type == VAL_INTERNAL)
#define IS_OBJ(value)        ((value).type == VAL_OBJ)
#define IS_ERR(value)        ((value).type == VAL_ERR)

#define AS_BOOL(value)       ((value).as.boolean)
#define AS_NUMBER(value)     ((value).as.number)
#define AS_INTERNAL(value)   ((value).as.internal)
#define AS_OBJ(value)        ((value).as.obj)
#define AS_ERR(value)        ((value).as.err)

#define BOOL_VAL(value)      ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL              ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value)    ((Value){VAL_NUMBER, {.number = value}})
#define INTERNAL_VAL(value)  ((Value){VAL_INTERNAL, {.internal = value}})
#define OBJ_VAL(object)      ((Value){VAL_OBJ, {.obj = (Obj*)object}})
#define ERR_VAL(value)       ((Value){VAL_ERR, {.err = value}})
// clang-format on

typedef struct {
  int capacity;
  int count;
  Value *values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
bool popValueArray(ValueArray *array, Value *value);
void printValue(Value value);

#endif
