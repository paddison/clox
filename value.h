#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include <stdint.h>

typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef struct ObjConstString ObjConstString;
typedef struct ObjArray ObjArray;

typedef uint8_t InternalNum;

typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_NUMBER,
  VAL_INTERNAL,
  VAL_OBJ,
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
    Obj *obj;
    InternalNum internal;
  } as;
} Value;

// clang-format off
#define IS_BOOL(value)       ((value).type == VAL_BOOL)
#define IS_NIL(value)        ((value).type == VAL_NIL)
#define IS_NUMBER(value)     ((value).type == VAL_NUMBER)
#define IS_INTERNAL(value)   ((value).type == VAL_INTERNAL)
#define IS_OBJ(value)        ((value).type == VAL_OBJ)

#define AS_BOOL(value)       ((value).as.boolean)
#define AS_NUMBER(value)     ((value).as.number)
#define AS_INTERNAL(value)   ((value).as.internal)
#define AS_OBJ(value)        ((value).as.obj)

#define BOOL_VAL(value)      ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL              ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value)    ((Value){VAL_NUMBER, {.number = value}})
#define INTERNAL_VAL(value)  ((Value){VAL_INTERNAL, {.internal = value}})
#define OBJ_VAL(object)      ((Value){VAL_OBJ, {.obj = (Obj*)object}})
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
