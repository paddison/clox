#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "myalloc.h"
#include "vm.h"

static void chunkTest();
static void challenge15_1();

int main(int argc, const char *argv[]) {
  initVM();
  Chunk chunk;
  initChunk(&chunk);

  int constant = addConstant(&chunk, 1.2);
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constant, 123);

  constant = addConstant(&chunk, 3.4);
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constant, 123);

  writeChunk(&chunk, OP_ADD, 123);

  constant = addConstant(&chunk, 5.6);
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constant, 123);

  writeChunk(&chunk, OP_DIVIDE, 123);
  writeChunk(&chunk, OP_NEGATE, 123);

  writeChunk(&chunk, OP_RETURN, 123);

  disassembleChunk(&chunk, "test chunk");
  interpret(&chunk);
  // chunkTest();
  challenge15_1();
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

static void challenge15_1() {
  /* 1 * 2 + 3 */
  Chunk chunk;
  initChunk(&chunk);
  int constant = addConstant(&chunk, 1);
  writeChunk(&chunk, OP_CONSTANT, 1);
  writeChunk(&chunk, constant, 1);

  constant = addConstant(&chunk, 2);
  writeChunk(&chunk, OP_CONSTANT, 1);
  writeChunk(&chunk, constant, 1);

  writeChunk(&chunk, OP_MULTIPLY, 1);

  constant = addConstant(&chunk, 3);
  writeChunk(&chunk, OP_CONSTANT, 1);
  writeChunk(&chunk, constant, 1);

  writeChunk(&chunk, OP_ADD, 1);

  writeChunk(&chunk, OP_RETURN, 1);

  disassembleChunk(&chunk, "1 * 2 + 3");
  interpret(&chunk);

  /* 1 + 2 * 3 */
  Chunk chunk2;
  initChunk(&chunk2);

  constant = addConstant(&chunk2, 1);
  writeChunk(&chunk2, OP_CONSTANT, 1);
  writeChunk(&chunk2, constant, 1);

  constant = addConstant(&chunk2, 2);
  writeChunk(&chunk2, OP_CONSTANT, 1);
  writeChunk(&chunk2, constant, 1);

  constant = addConstant(&chunk2, 3);
  writeChunk(&chunk2, OP_CONSTANT, 1);
  writeChunk(&chunk2, constant, 1);

  writeChunk(&chunk2, OP_MULTIPLY, 1);
  writeChunk(&chunk2, OP_ADD, 1);

  writeChunk(&chunk2, OP_RETURN, 1);

  disassembleChunk(&chunk2, "1 + 2 * 3");
  interpret(&chunk2);

  /* 3 - 2 - 1 */
  Chunk chunk3;
  initChunk(&chunk3);

  constant = addConstant(&chunk3, 3);
  writeChunk(&chunk3, OP_CONSTANT, 1);
  writeChunk(&chunk3, constant, 1);

  constant = addConstant(&chunk3, 2);
  writeChunk(&chunk3, OP_CONSTANT, 1);
  writeChunk(&chunk3, constant, 1);

  writeChunk(&chunk3, OP_SUBTRACT, 1);

  constant = addConstant(&chunk3, 1);
  writeChunk(&chunk3, OP_CONSTANT, 1);
  writeChunk(&chunk3, constant, 1);

  writeChunk(&chunk3, OP_SUBTRACT, 1);

  writeChunk(&chunk3, OP_RETURN, 1);

  disassembleChunk(&chunk3, "3 - 2 - 1");
  interpret(&chunk3);

  /* 1 + 2 * 3 - 4 / -5 */
  Chunk chunk4;
  initChunk(&chunk4);

  constant = addConstant(&chunk4, 1);
  writeChunk(&chunk4, OP_CONSTANT, 1);
  writeChunk(&chunk4, constant, 1);

  constant = addConstant(&chunk4, 2);
  writeChunk(&chunk4, OP_CONSTANT, 1);
  writeChunk(&chunk4, constant, 1);

  constant = addConstant(&chunk4, 2);
  writeChunk(&chunk4, OP_CONSTANT, 1);
  writeChunk(&chunk4, constant, 1);

  writeChunk(&chunk4, OP_MULTIPLY, 1);
  writeChunk(&chunk4, OP_ADD, 1);

  constant = addConstant(&chunk4, 4);
  writeChunk(&chunk4, OP_CONSTANT, 1);
  writeChunk(&chunk4, constant, 1);

  constant = addConstant(&chunk4, 5);
  writeChunk(&chunk4, OP_CONSTANT, 1);
  writeChunk(&chunk4, constant, 1);

  writeChunk(&chunk4, OP_NEGATE, 1);
  writeChunk(&chunk4, OP_DIVIDE, 1);
  writeChunk(&chunk4, OP_SUBTRACT, 1);

  writeChunk(&chunk4, OP_RETURN, 1);

  disassembleChunk(&chunk4, "1 + 2 * 3 - 4 / -5");
  interpret(&chunk4);

  freeChunk(&chunk);
  freeChunk(&chunk2);
  freeChunk(&chunk3);
  freeChunk(&chunk4);
}
