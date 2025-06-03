#include <stdint.h>
#include <stdlib.h>

#include "chunk.h"
#include "lines.h"
#include "memory.h"

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

int addConstant(Chunk *chunk, Value value) {
  writeValueArray(&chunk->constants, value);
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
void writeConstant(Chunk *chunk, Value value, int line) {
  const int address = addConstant(chunk, value);

  if (address >= UINT8_MAX) {
    writeChunk(chunk, OP_CONSTANT_LONG, line);
    writeChunk(chunk, (address & 0xFF), line);
    writeChunk(chunk, ((address >> 8) & 0xFF), line);
    writeChunk(chunk, ((address >> 16) & 0xFF), line);
  } else {
    writeChunk(chunk, OP_CONSTANT, line);
    writeChunk(chunk, address, line);
  }
}
