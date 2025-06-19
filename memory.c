#include <stdlib.h>

#include "memory.h"
#include "myalloc.h"

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    myFree(pointer);
    return NULL;
  }

  void *result = myRealloc(pointer, newSize, 1);
  if (result == NULL)
    exit(1);
  return result;
}
