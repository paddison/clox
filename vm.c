#include <stdio.h>

#include "chunk.h"
#include "common.h"
#include "vm.h"


VM vm;

void initVM() {
}

void freeVM() {
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_LONG()             \
    (uint32_t)READ_BYTE()       \
  | (uint32_t)READ_BYTE() <<  8 \
  | (uint32_t)READ_BYTE() << 16 
#define READ_CONSTANT_LONG() (vm.chunk->constants.values[READ_LONG()])


  for (;;) {
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_RETURN: {
        return INTERPRET_OK;
      }
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        printValue(constant);
        printf("\n");
        break;
      }
      case OP_CONSTANT_LONG: {
        Value constant = READ_CONSTANT_LONG();

        printValue(constant);
        printf("\n");
        break;
      }
    }
  }

#undef READ_BYTE
}

InterpretResult interpret(Chunk* chunk) {
  vm.chunk = chunk;
  vm.ip = vm.chunk->code;
  return run();
}


