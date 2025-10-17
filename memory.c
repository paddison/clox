#include <stdlib.h>

#include "memory.h"
#include "myalloc.h"
#include "vm.h"

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    myFree(pointer);
    return NULL;
  }

  // void *result = realloc(pointer, newSize);
  void *result = myRealloc(pointer, newSize, 1);
  if (result == NULL)
    exit(1);
  return result;
}

static void freeObject(Obj *object) {
  switch (object->type) {
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
