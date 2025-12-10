#include <stdlib.h>

#include "memory.h"
#include "myalloc.h"
#include "object.h"
#include "value.h"
#include "vm.h"

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    myFree(pointer);
    return NULL;
  }

#ifdef USE_CUSTOM_MALLOC
  void *result = myRealloc(pointer, newSize, 1);
#else
  void *result = realloc(pointer, newSize);
#endif
  if (result == NULL)
    exit(1);
  return result;
}

static void freeObject(Obj *object) {
  switch (object->type) {
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    freeChunk(&function->chunk);
    FREE(ObjFunction, object);
    break;
  }
  case OBJ_NATIVE:
    FREE(ObjNative, object);
    break;
  case OBJ_STRING: {
    ObjString *string = (ObjString *)object;
    reallocate(string, sizeof(ObjString) + string->length + 1, 0);
    break;
  }
  case OBJ_CONST_STRING: {
    ObjConstString *string = (ObjConstString *)object;
    reallocate(string, sizeof(ObjConstString), 0);
    break;
  }
  case OBJ_ARRAY: {
    ObjArray *array = (ObjArray *)object;
    freeValueArray(&array->array);
    reallocate(array, sizeof(ObjArray), 0);
    break;
  }
  case OBJ_CLOSURE: {
    ObjClosure *closure = (ObjClosure *)object;
    FREE_ARRAY(ObjUpvalue *, closure->upvalues, closure->upvalueCount);
    FREE(ObjClosure, object);
    break;
  }
  case OBJ_UPVALUE:
    break;
  }
}

void freeObjects() {
  Obj *object = vm.objects;
  while (object != NULL) {
    Obj *next = object->next;
    freeObject(object);
    object = next;
  }
}
