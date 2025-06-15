#ifndef clox_myalloc_h
#define clox_myalloc_h

#include <stddef.h>
#include <stdint.h>

typedef enum AllocErr {
  ok,
  outOfMemory,
} AllocErr;

#endif
