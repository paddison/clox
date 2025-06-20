#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "myalloc.h"
#include "vm.h"

static void chunkTest();

int main(int argc, const char *argv[]) {
  initVM();
  Chunk chunk;
  initChunk(&chunk);

  int constant = addConstant(&chunk, 1.2);
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constant, 123);

  writeChunk(&chunk, OP_RETURN, 123);

  disassembleChunk(&chunk, "test chunk");
  interpret(&chunk);
  // chunkTest();
  freeVM();
  freeChunk(&chunk);

  return 0;
}

static void chunkTest() {
  /* check that malloc and free "kind of" work */
  char *ptr = myMalloc(8, sizeof(char));
  myFree(ptr);

  /* check that constants are written correctly */
  Chunk chunk;
  initChunk(&chunk);

  for (int i = 0; i < 1024; ++i) {
    writeConstant(&chunk, i, i / 2);
  }
  writeChunk(&chunk, OP_RETURN, 123);

  // disassembleChunk(&chunk, "test chunk 2");
  interpret(&chunk);

  freeChunk(&chunk);
}
