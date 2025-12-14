#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "lines.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

VM vm;

static Value clockNative(int argCount, Value *args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value sqrtNative(int argCount, Value *args) {
  Value n = args[0];
  if (!IS_NUMBER(n)) {
    return ERR_VAL("Invalid argument type for sqrt.");
  }
  return NUMBER_VAL(sqrt(AS_NUMBER(n)));
}

Native natives[NUMBER_OF_NATIVES] = {
    {clockNative, "clock", 0},
    {sqrtNative, "sqrt", 1},
};

static void resetStack() {
  vm.stackTop = vm.stack;
  vm.frameCount = 0;
  vm.openUpvalues = NULL;
}

static void runtimeError(const uint8_t *const ip, const char *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame *frame = &vm.frames[i];
    ObjFunction *function = frame->closure->function;
    size_t instruction = ip - function->chunk.code - 1;
    fprintf(
        stderr, "[line %d] in ",
        getLine(&frame->closure->function->chunk.lineEncoding, instruction));
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }
  resetStack();
}

static void defineNative(Native native) {
  push(OBJ_VAL(copyString(native.name, (int)strlen(native.name))));
  push(OBJ_VAL(newNative(native.fn, native.arity)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  pop();
  pop();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  vm.bytesAllocated = 0;
  vm.nextGC = 1024 * 1024;

  vm.grayCount = 0;
  vm.grayCapacity = 0;
  vm.grayStack = NULL;

  initTable(&vm.globals);
  initTable(&vm.strings);

  for (int i = 0; i < NUMBER_OF_NATIVES; i++) {
    defineNative(natives[i]);
  }
}

void freeVM() {
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  freeObjects();
}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

static Value peek(int distance) { return vm.stackTop[-1 - distance]; }

static bool call(ObjClosure *closure, int argCount, const uint8_t *ip) {
  if (argCount != closure->function->arity) {
    runtimeError(ip, "Expected %d arguments but got %d.",
                 closure->function->arity, argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    runtimeError(ip, "Stack overflow.");
    return false;
  }

  CallFrame *frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;
  frame->slots = vm.stackTop - argCount - 1;
  return true;
}

static bool callNative(ObjNative *native, int argCount,
                       const uint8_t *const ip) {
  if (native->arity != argCount) {
    runtimeError(ip, "Incorrect number of arguments.");
    return false;
  }

  Value result = native->function(argCount, vm.stackTop - argCount);

  if (IS_ERR(result)) {
    runtimeError(ip, AS_ERR(result));
    return false;
  }

  vm.stackTop -= argCount + 1;
  push(result);
  return true;
}

static bool callValue(uint8_t *ip, Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
    case OBJ_CLOSURE:
      return call(AS_CLOSURE(callee), argCount, ip);
    case OBJ_NATIVE: {
      return callNative(AS_NATIVE(callee), argCount, ip);
    }
    default:
      break;
    }
  }
  runtimeError(ip, "Can only call functions and classes.");
  return false;
}

static ObjUpvalue *captureUpvalue(Value *local) {
  ObjUpvalue *prevUpvalue = NULL;
  ObjUpvalue *upvalue = vm.openUpvalues;
  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

  ObjUpvalue *createdUpvalue = newUpvalue(local);
  createdUpvalue->next = upvalue;

  if (prevUpvalue == NULL) {
    vm.openUpvalues = createdUpvalue;
  } else {
    prevUpvalue->next = createdUpvalue;
  }

  return createdUpvalue;
}

static void closeUpvalues(Value *last) {
  while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
    ObjUpvalue *upvalue = vm.openUpvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
}

static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
#define CONCAT(a, b, string)                                                   \
  do {                                                                         \
    int length = a->length + stringB->length;                                  \
    string = allocateEmptyString(length);                                      \
    memcpy(string->chars, a->chars, a->length);                                \
    memcpy(string->chars + a->length, b->chars, b->length);                    \
    string->chars[length] = '\0';                                              \
  } while (false)

  ObjString *string;
  Value b = peek(0);
  Value a = peek(1);

  if (isObjType(a, OBJ_STRING) && isObjType(b, OBJ_STRING)) {
    ObjString *stringA = AS_STRING(a);
    ObjString *stringB = AS_STRING(b);
    CONCAT(stringA, stringB, string);
  } else if (isObjType(a, OBJ_STRING) && isObjType(b, OBJ_CONST_STRING)) {
    ObjString *stringA = AS_STRING(a);
    ObjConstString *stringB = AS_CONST_STRING(b);
    CONCAT(stringA, stringB, string);
  } else if (isObjType(a, OBJ_CONST_STRING) && isObjType(b, OBJ_STRING)) {
    ObjConstString *stringA = AS_CONST_STRING(a);
    ObjString *stringB = AS_STRING(b);
    CONCAT(stringA, stringB, string);
  } else {
    ObjConstString *stringA = AS_CONST_STRING(a);
    ObjConstString *stringB = AS_CONST_STRING(b);
    CONCAT(stringA, stringB, string);
  }

  pop();
  pop();

  push(OBJ_VAL(string));
#undef CONCAT
}

static InterpretResult run() {
  CallFrame *frame = &vm.frames[vm.frameCount - 1];
  uint8_t *ip = frame->ip;

#define READ_BYTE() (*ip++)

#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))

#define READ_CONSTANT()                                                        \
  (frame->closure->function->chunk.constants.values[READ_BYTE()])

#define READ_GLOBAL_CONSTANT()                                                 \
  (vm.frames[0].closure->function->chunk.constants.values[READ_BYTE()])

#define READ_STRING() AS_STRING(READ_CONSTANT())

#define READ_GLOBAL_STRING() AS_STRING(READ_GLOBAL_CONSTANT())

#define READ_LONG()                                                            \
  (ip += 3, (uint32_t)((ip[-3] << 16) | (ip[-2] << 8) | ip[-1]))

#define READ_CONSTANT_LONG()                                                   \
  (frame->closure->function->chunk.constants.values[READ_LONG()])

#define BINARY_OP(valueType, op)                                               \
  do {                                                                         \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {                          \
      runtimeError(ip, "Operands must be numbers.");                           \
      return INTERPRET_RUNTIME_ERROR;                                          \
    }                                                                          \
    double b = AS_NUMBER(pop());                                               \
    Value *a = vm.stackTop - 1;                                                \
    *a = valueType(AS_NUMBER(*a) op b);                                        \
  } while (false)

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (Value *slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(&frame->closure->function->chunk,
                           (int)(ip - frame->closure->function->chunk.code));
#endif
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      push(constant);
      break;
    }
    case OP_CONSTANT_LONG: {
      Value constant = READ_CONSTANT_LONG();
      push(constant);
      break;
    }
    case OP_NIL:
      push(NIL_VAL);
      break;
    case OP_TRUE:
      push(BOOL_VAL(true));
      break;
    case OP_FALSE:
      push(BOOL_VAL(false));
      break;
    case OP_POP:
      pop();
      break;
    case OP_GET_LOCAL: {
      uint8_t slot = READ_BYTE();
      push(frame->slots[slot]);
      break;
    }
    case OP_GET_GLOBAL: {
      ObjString *name = READ_GLOBAL_STRING();
      Value value;
      if (!tableGet(&vm.globals, name, &value)) {
        runtimeError(ip, "Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      push(value);
      break;
    }
    case OP_DEFINE_GLOBAL: {
      ObjString *name = READ_GLOBAL_STRING();
      tableSet(&vm.globals, name, peek(0));
      pop();
      break;
    }
    case OP_SET_LOCAL: {
      uint8_t slot = READ_BYTE();
      frame->slots[slot] = peek(0);
      break;
    }
    case OP_SET_GLOBAL: {
      ObjString *name = READ_GLOBAL_STRING();
      if (tableSet(&vm.globals, name, peek(0))) {
        tableDelete(&vm.globals, name);
        runtimeError(ip, "Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_GET_UPVALUE: {
      uint8_t slot = READ_BYTE();
      push(*frame->closure->upvalues[slot]->location);
      break;
    }
    case OP_SET_UPVALUE: {
      uint8_t slot = READ_BYTE();
      *frame->closure->upvalues[slot]->location = peek(0);
      break;
    }
    case OP_EQUAL: {
      Value a = pop();
      Value b = pop();
      push(BOOL_VAL(valuesEqual(a, b)));
      break;
    }
    case OP_GREATER:
      BINARY_OP(BOOL_VAL, >);
      break;
    case OP_LESS:
      BINARY_OP(BOOL_VAL, <);
      break;
    case OP_ADD: {
      if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
        concatenate();
      } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
        double b = AS_NUMBER(pop());
        double a = AS_NUMBER(pop());
        push(NUMBER_VAL(a + b));
      } else {
        runtimeError(ip, "Operands must be two numbers or two strings.");
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_SUBTRACT:
      BINARY_OP(NUMBER_VAL, -);
      break;
    case OP_MULTIPLY:
      BINARY_OP(NUMBER_VAL, *);
      break;
    case OP_DIVIDE:
      BINARY_OP(NUMBER_VAL, /);
      break;
    case OP_NOT:
      push(BOOL_VAL(isFalsey(pop())));
      break;
    case OP_JUMP: {
      uint16_t offset = READ_SHORT();
      ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      uint16_t offset = READ_SHORT();
      if (isFalsey(peek(0)))
        ip += offset;
      break;
    }
    case OP_LOOP: {
      uint16_t offset = READ_SHORT();
      ip -= offset;
      break;
    }
    case OP_SWITCH_COMPARE: {
      Value switchExpression = peek(1);
      Value caseExpression = pop();
      push(BOOL_VAL(valuesEqual(switchExpression, caseExpression)));
      break;
    }
    case OP_CALL: {
      int argCount = READ_BYTE();
      if (!callValue(ip, peek(argCount), argCount)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame->ip = ip;
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_CLOSURE: {
      ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
      ObjClosure *closure = newClosure(function);
      push(OBJ_VAL(closure));
      for (int i = 0; i < closure->upvalueCount; i++) {
        uint8_t isLocal = READ_BYTE();
        uint8_t index = READ_BYTE();
        if (isLocal) {
          closure->upvalues[i] = captureUpvalue(frame->slots + index);
        } else {
          closure->upvalues[i] = frame->closure->upvalues[index];
        }
      }
      break;
    }
    case OP_CLOSE_UPVALUE:
      closeUpvalues(vm.stackTop - 1);
      pop();
      break;
    case OP_RETURN: {
      Value result = pop();
      closeUpvalues(frame->slots);
      vm.frameCount--;
      if (vm.frameCount == 0) {
        pop();
        return INTERPRET_OK;
      }

      vm.stackTop = frame->slots;
      push(result);
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_NEGATE: {
      if (!IS_NUMBER(peek(0))) {
        runtimeError(ip, "Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }
      Value *n = vm.stackTop - 1;
      n->as.number = -(AS_NUMBER(*n));
      break;
    }
    case OP_PRINT: {
      printValue(pop());
      printf("\n");
      break;
    }
    }
  }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef READ_LONG
#undef READ_CONSTANT_LONG
#undef BINARY_OP
}

InterpretResult interpret(const char *source) {
  ObjFunction *function = compile(source);
  if (function == NULL)
    return INTERPRET_COMPILE_ERROR;

  push(OBJ_VAL(function));
  ObjClosure *closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  call(closure, 0, vm.frames[0].ip);

  return run();
}
