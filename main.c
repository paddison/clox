#include "chunk.h"
#include "common.h"
#include "debug.h"

int main(int argc, const char *argv[]) {
  Chunk chunk;
  initChunk(&chunk);

  int constant = addConstant(&chunk, 1.2);
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constant, 123);

  writeChunk(&chunk, OP_RETURN, 123);
  writeChunk(&chunk, OP_RETURN, 123);
  int constant2 = addConstant(&chunk, 1.5);
  writeChunk(&chunk, OP_CONSTANT, 124);
  writeChunk(&chunk, constant2, 124);
  writeChunk(&chunk, OP_RETURN, 124);
  writeChunk(&chunk, OP_RETURN, 125);

  disassembleChunk(&chunk, "test chunk");

  Chunk chunk2;
  initChunk(&chunk);

  for (int i = 0; i < 1024; ++i) {
    writeConstant(&chunk, i, i / 2);
  }

  disassembleChunk(&chunk, "test chunk 2");

  freeChunk(&chunk);
  freeChunk(&chunk2);
  return 0;
}
