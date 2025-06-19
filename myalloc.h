#ifndef clox_myalloc_h
#define clox_myalloc_h

#include <stddef.h>
#include <stdint.h>

void *myMalloc(size_t size, size_t sizeOfType);
void myFree(void* memory);
void *myRealloc(void *oldPtr, size_t size, size_t sizeOfType);

#endif
