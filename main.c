#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "myalloc.h"

int main(int argc, const char *argv[]) {
  char *ptr = myMalloc(8, sizeof(char));
  myFree(ptr);
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
  initChunk(&chunk2);

  for (int i = 0; i < 1024; ++i) {
    writeConstant(&chunk2, i, i / 2);
  }

  disassembleChunk(&chunk2, "test chunk 2");

  freeChunk(&chunk);
  //freeChunk(&chunk2);
  return 0;
}
