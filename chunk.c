#include <stdint.h>
#include <stdlib.h>

#include "chunk.h"
#include "lines.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  initLinesEncoding(&chunk->lineEncoding);
  initValueArray(&chunk->constants);
}

void freeChunk(Chunk *chunk) {
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  freeLinesEncoding(&chunk->lineEncoding);
  freeValueArray(&chunk->constants);
  initChunk(chunk);
}

void writeChunk(Chunk *chunk, uint8_t byte, int line) {
  if (chunk->capacity < chunk->count + 1) {
    int oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code =
        GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
  }

  chunk->code[chunk->count] = byte;
  writeLinesEncoding(&chunk->lineEncoding, line);
  chunk->count++;
}

InternalNum addConstant(Chunk *chunk, Value value) {
  push(value);
  writeValueArray(&chunk->constants, value);
  pop();
  return chunk->constants.count - 1;
}

/**
 * Stores the constant in little endian. Because my Laptop uses it :>
 *
 * Example:
 *
 * The number 0A0B0C will be stored in the chunk in the following way:
 *
 *                0x0  0x1  0x2  0x3
 * /---------------------------------/
 * / OP_CONSTANT_LONG | 0C | 0B | 0A /
 * /---------------------------------/
 */
InternalNum writeConstant(Chunk *chunk, Value value, int line) {
  const int address = addConstant(chunk, value);

  return address;
}
