#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType)                                         \
  (type *)allocateObject(sizeof(type), objectType)

static Obj *allocateObject(size_t size, ObjType type) {
  Obj *object = (Obj *)reallocate(NULL, 0, size);
  object->type = type;

  object->next = vm.objects;
  vm.objects = object;
  return object;
}

static ObjString *allocateString(const char *chars, int length, uint32_t hash) {
  ObjString *string =
      (ObjString *)allocateObject(sizeof(ObjString) + length + 1, OBJ_STRING);
  string->length = length;
  memcpy(string->chars, chars, length);
  string->chars[length] = '\0';
  string->hash = hash;
  tableSet(&vm.strings, string, NIL_VAL);
  return string;
}

static uint32_t hashString(const char *key, int length) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

/**
 * Allocate an empty string with the specified length.
 */
ObjString *allocateEmptyString(const int length) {
  ObjString *string =
      (ObjString *)allocateObject(sizeof(ObjString) + length + 1, OBJ_STRING);
  string->length = length;
  return string;
}

ObjString *takeString(char *chars, int length) {
  uint32_t hash = hashString(chars, length);
  ObjString *interned = tableFindString(&vm.strings, chars, length, hash);

  if (interned != NULL) {
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }

  return allocateString(chars, length, hash);
}

ObjString *copyString(const char *chars, int length) {
  uint32_t hash = hashString(chars, length);
  ObjString *interned = tableFindString(&vm.strings, chars, length, hash);

  if (interned != NULL)
    return interned;

  return allocateString(chars, length, hash);
}

ObjConstString *constString(const char *chars, int length) {
  ObjConstString *string = ALLOCATE_OBJ(ObjConstString, OBJ_CONST_STRING);
  string->length = length;
  string->chars = chars;
  return string;
}

ObjArray *allocateEmptyArray() {
  ObjArray *array = ALLOCATE_OBJ(ObjArray, OBJ_ARRAY);
  initValueArray(&array->array);
  return array;
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  case OBJ_CONST_STRING: {
    ObjConstString *string = AS_CONST_STRING(value);
    printf("%.*s", string->length, string->chars);
    break;
  }
  case OBJ_ARRAY: {
    ObjArray *array = AS_ARRAY(value);
    int count = array->array.count;

    printf("[");

    for (int i = 0; i < count - 1; i++) {
      printValue(array->array.values[i]);
      printf(", ");
    }

    printValue(array->array.values[count - 1]);
    printf("]");
  }
  }
}
