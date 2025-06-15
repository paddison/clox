#ifndef clox_myalloc_h
#define clox_myalloc_h

#include <stddef.h>
#include <stdint.h>

typedef enum AllocErr {
  ok,
  outOfMemory,
} AllocErr;

void *myMalloc(size_t size, size_t sizeOfType);
void myFree(void* memory);

#endif
