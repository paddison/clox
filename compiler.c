#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "scanner.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

#define LOCAL_MAX 512
#define NO_LOOP (-1)

typedef struct {
  Token current;
  Token previous;
  bool hadError;
  bool panicMode;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_UNARY,      // ! -
  PREC_CALL,       // . ()
  PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

typedef struct {
  Token name;
  int depth;
  bool isConstant;
  bool isCaptured;
} Local;

typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;

typedef enum {
  TYPE_FUNCTION,
  TYPE_INITIALIZER,
  TYPE_METHOD,
  TYPE_SCRIPT,
} FunctionType;

typedef struct {
  Token name;
  bool isConstant;
  InternalNum indexInCurrentChunkValues;
} Global;

typedef struct {
  int loopStart;
  int loopScope;
} LoopInfo;

typedef struct {
  // Globals
  Table globalVariableNames;
  Global globals[UINT8_COUNT];
  int globalCount;
  bool isInitialized;
} Globals;

typedef struct Compiler {
  // Each compiler compiles a function.
  struct Compiler *enclosing;
  // Function
  ObjFunction *function;
  FunctionType type;
  // Locals
  Local locals[LOCAL_MAX];
  int localCount;
  Upvalue upvalues[UINT8_COUNT];
  int scopeDepth;
  Table variablesAtIndex;
  // Loop Info (for continue statement)
  LoopInfo currentLoop;
} Compiler;

typedef struct ClassCompiler {
  struct ClassCompiler *enclosing;
} ClassCompiler;

// forward declarations
static void unary(bool canAssign);
static void binary(bool canAssign);
static void call(bool canAssign);
static void dot(bool canAssign);
static void number(bool canAssign);
static void string(bool canAssign);
static void variable(bool canAssign);
static void literal(bool canAssign);
static void expression();
static void grouping(bool canAssign);
static void statement();
static void declaration();
static void and_(bool canAssign);
static void or_(bool canAssign);
static void this_(bool canAssign);
static void namedVariable(Token name, bool canAssign);
static void addLocalToTable(Token name);
static void addLocal(Token name, bool isConstant, int depth);

// clang-format off
ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {this_,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};
// clang-format on

Parser parser;
Compiler *current = NULL;
ClassCompiler *currentClass = NULL;
Globals globals;

// defined in vm.c
extern Native natives[NUMBER_OF_NATIVES];

static Chunk *currentChunk() { return &current->function->chunk; }

static void errorAt(Token *token, const char *message) {
  if (parser.panicMode)
    return;

  parser.panicMode = true;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char *message) { errorAt(&parser.previous, message); }

static void fatal(const char *message) {
  Token *token = &parser.previous;
  fprintf(stderr, "[line %d] Fatal: %s", token->line, message);
  exit(4);
}

static void errorAtCurrent(const char *message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR)
      break;

    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char *message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

static bool check(TokenType type) { return parser.current.type == type; }

static bool match(TokenType type) {
  if (!check(type))
    return false;
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);

  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX)
    error("Loop body too large.");

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  emitByte(0xff);
  emitByte(0xff);
  return currentChunk()->count - 2;
}

static void emitReturn() {
  if (current->type == TYPE_INITIALIZER) {
    emitBytes(OP_GET_LOCAL, 0);
  } else {
    emitByte(OP_NIL);
  }
  emitByte(OP_RETURN);
}

static InternalNum makeConstant(Value value) {
  return writeConstant(currentChunk(), value, parser.previous.line);
}

static void emitConstant(Value value) {
  int address = makeConstant(value);
  int line = parser.previous.line;
  Chunk *chunk = currentChunk();

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

static void patchJump(int offset) {
  // -2 to adjust for the bytecode of the jump offset itself.
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler *compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function = NULL;
  compiler->type = type;

  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  compiler->function = newFunction();
  compiler->currentLoop.loopStart = NO_LOOP;
  compiler->currentLoop.loopScope = 0;
  initTable(&compiler->variablesAtIndex);
  current = compiler;
  if (type != TYPE_SCRIPT) {
    current->function->name =
        copyString(parser.previous.start, parser.previous.length);
  }

  // Local *local = &current->locals[current->localCount++];
  Token local;

  // local->depth = 0;
  // local->isCaptured = false;
  // local->isConstant = false;
  if (type != TYPE_FUNCTION) {
    local.start = "this";
    local.length = 4;
  } else {
    local.start = "";
    local.length = 0;
  }

  addLocal(local, false, 0);
  // addLocalToTable(local->name);
}

static ObjFunction *endCompiler() {
  emitReturn();
  ObjFunction *function = current->function;
  freeTable(&current->variablesAtIndex);
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), function->name != NULL
                                         ? function->name->chars
                                         : "<script>");
  }
#endif

  current = current->enclosing;
  return function;
}

static void beginScope() { current->scopeDepth++; }

static void endScope() {
  current->scopeDepth--;

  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
    if (current->locals[current->localCount - 1].isCaptured) {
      emitByte(OP_CLOSE_UPVALUE);
    } else {
      emitByte(OP_POP);
    }
    Local *local = &current->locals[current->localCount - 1];
    ObjString *localName = copyString(local->name.start, local->name.length);
    Value array;

    if (tableGet(&current->variablesAtIndex, localName, &array)) {
      Value _;
      popValueArray(&AS_ARRAY(array)->array, &_);
    }

    current->localCount--;
  }
}

static LoopInfo beginLoop(int loopStart) {
  LoopInfo previousLoop = current->currentLoop;

  current->currentLoop.loopStart = loopStart;
  current->currentLoop.loopScope = current->scopeDepth;

  return previousLoop;
}

static void exitLoop(LoopInfo previousLoop) {
  current->currentLoop = previousLoop;
}

static ParseRule *getRule(TokenType type) { return &rules[type]; }

static void parsePrecedence(Precedence precedence) {
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }
}

static bool getGlobal(Token *name, Global *global) {
  ObjString *variableName = copyString(name->start, name->length);
  Value index;
  bool wasFound = false;

  if (tableGet(&globals.globalVariableNames, variableName, &index)) {
    *global = globals.globals[AS_INTERNAL(index)];
    wasFound = true;
  }

  return wasFound;
}

static InternalNum addGlobal(Token *name, bool isConstant) {
  ObjString *variableName = copyString(name->start, name->length);
  Value variableNameAsValue = OBJ_VAL(variableName);
  InternalNum indexRaw = makeConstant(variableNameAsValue);
  Global *global = &globals.globals[globals.globalCount++];

  global->name = *name;
  global->indexInCurrentChunkValues = indexRaw;
  global->isConstant = isConstant;

  tableSet(&globals.globalVariableNames, variableName,
           INTERNAL_VAL(globals.globalCount - 1));

  return indexRaw;
}

// Returns where it is in the constantArray
static InternalNum identifierConstant(Token *name, bool isConstant) {
  Value index;
  Global global;

  printf("%.*s\n", name->length, name->start);

  if (getGlobal(name, &global)) {
    return global.indexInCurrentChunkValues;
  } else {
    return addGlobal(name, isConstant);
  }

  // Should never be reached.
  assert(false);
}

static bool identifiersEqual(Token *a, Token *b) {
  if (a->length != b->length)
    return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

static bool lookupIndexOfLocal(Token name, Compiler *compiler,
                               InternalNum *index) {
  ObjString *string = copyString(name.start, name.length);
  Value indicesOfLocalValue;
  bool isKnown = false;
  if (tableGet(&compiler->variablesAtIndex, string, &indicesOfLocalValue)) {
    ObjArray *indicesOfLocal = AS_ARRAY(indicesOfLocalValue);
    if (indicesOfLocal->array.count > 0) {
      Value foundIndex =
          indicesOfLocal->array.values[indicesOfLocal->array.count - 1];
      *index = AS_INTERNAL(foundIndex);
      isKnown = true;
    }
  }

  return isKnown;
}

static int resolveLocal(Compiler *compiler, Token *name) {

  InternalNum index;
  int foundIndex = -1;

  if (lookupIndexOfLocal(*name, compiler, &index)) {
    Local *local = &compiler->locals[index];
    if (local->depth == -1) {
      error("Can't read local variable in its own initializer.");
    }

    foundIndex = (int)index;
  }

  return foundIndex;
}

static int addUpvalue(Compiler *compiler, uint8_t index, bool isLocal) {
  int upvalueCount = compiler->function->upvalueCount;

  for (int i = 0; i < upvalueCount; i++) {
    Upvalue *upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    error("Too many closure variables in function.");
    return 0;
  }

  compiler->upvalues[upvalueCount].isLocal = isLocal;
  compiler->upvalues[upvalueCount].index = index;
  return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler *compiler, Token *name) {
  if (compiler->enclosing == NULL)
    return -1;

  int local = resolveLocal(compiler->enclosing, name);

  if (local != -1) {
    compiler->enclosing->locals[local].isCaptured = true;
    return addUpvalue(compiler, (uint8_t)local, true);
  }

  int upvalue = resolveUpvalue(compiler->enclosing, name);
  if (upvalue != -1) {
    return addUpvalue(compiler, (uint8_t)upvalue, false);
  }

  return -1;
}

static void addLocalToTable(Token name) {
  ObjString *string = copyString(name.start, name.length);
  Value string_val = OBJ_VAL(string);
  // push the string as it is not tracked anywhere.
  push(string_val);
  Value indicesOfLocal;
  Value index = INTERNAL_VAL(current->localCount);

  if (!tableGet(&current->variablesAtIndex, string, &indicesOfLocal)) {
    indicesOfLocal = OBJ_VAL(allocateEmptyArray());
    push(indicesOfLocal);
    tableSet(&current->variablesAtIndex, string, indicesOfLocal);
    pop();
  }
  // after the string has been added to the table, it will be tracked from
  // there. so it is safe to pop it again
  pop();

  if (!IS_ARRAY(indicesOfLocal)) {
    fatal("Internal compiler error during variable lookup.");
  }

  writeValueArray(&AS_ARRAY(indicesOfLocal)->array, index);
}

static void addLocal(Token name, bool isConstant, int depth) {
  if (current->localCount == UINT16_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  addLocalToTable(name);

  Local *local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = depth;
  local->isConstant = isConstant;
  local->isCaptured = false;
  printf("Local %.*s at depth: %d, localCount: %d\n", name.length, name.start,
         current->scopeDepth, current->localCount);
}

static void declareVariable(bool isConstant) {
  if (current->scopeDepth == 0)
    return;

  Token *name = &parser.previous;

  InternalNum index;

  if (lookupIndexOfLocal(*name, current, &index)) {
    Local *local = &current->locals[index];

    if (local->depth == current->scopeDepth &&
        identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }

  addLocal(*name, isConstant, -1);
}

static uint8_t parseVariable(const char *errorMessage, bool isConstant) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable(isConstant);
  if (current->scopeDepth > 0)
    return 0;

  return identifierConstant(&parser.previous, isConstant);
}

static void markInitialized() {
  if (current->scopeDepth == 0)
    return;
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
  uint8_t argCount = 0;

  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();
      argCount++;
      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }
    } while (match(TOKEN_COMMA));
  }

  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void and_(bool canAssign) {
  int endJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  parsePrecedence(PREC_AND);
  patchJump(endJump);
}

static void binary(bool canAssign) {
  TokenType operatorType = parser.previous.type;
  ParseRule *rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
  case TOKEN_BANG_EQUAL:
    emitBytes(OP_EQUAL, OP_NOT);
    break;
  case TOKEN_EQUAL_EQUAL:
    emitByte(OP_EQUAL);
    break;
  case TOKEN_GREATER:
    emitByte(OP_GREATER);
    break;
  case TOKEN_GREATER_EQUAL:
    emitBytes(OP_LESS, OP_NOT);
    break;
  case TOKEN_LESS:
    emitByte(OP_LESS);
    break;
  case TOKEN_LESS_EQUAL:
    emitBytes(OP_GREATER, OP_NOT);
    break;
  case TOKEN_PLUS:
    emitByte(OP_ADD);
    break;
  case TOKEN_MINUS:
    emitByte(OP_SUBTRACT);
    break;
  case TOKEN_STAR:
    emitByte(OP_MULTIPLY);
    break;
  case TOKEN_SLASH:
    emitByte(OP_DIVIDE);
    break;
  default:
    return; // Unreachable
  }
}

static void call(bool canAssign) {
  uint8_t argCount = argumentList();
  emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect proprety name after '.'.");
  uint8_t name = identifierConstant(&parser.previous, false);

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(OP_SET_PROPERTY, name);
  } else if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    emitBytes(OP_INVOKE, name);
    emitByte(argCount);
  } else {
    emitBytes(OP_GET_PROPERTY, name);
  }
}

static void literal(bool canAssign) {
  switch (parser.previous.type) {
  case TOKEN_FALSE:
    emitByte(OP_FALSE);
    break;
  case TOKEN_NIL:
    emitByte(OP_NIL);
    break;
  case TOKEN_TRUE:
    emitByte(OP_TRUE);
    break;
  default:
    return; // Unreachable.
  }
}

static void expression() { parsePrecedence(PREC_ASSIGNMENT); }

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
  Compiler compiler;
  initCompiler(&compiler, type);
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;
      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }
      uint8_t constant = parseVariable("Expect parameter name.", false);
      defineVariable(constant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();

  ObjFunction *function = endCompiler();
  emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

  for (int i = 0; i < function->upvalueCount; i++) {
    emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
    emitByte(compiler.upvalues[i].index);
  }
}

static void method() {
  consume(TOKEN_IDENTIFIER, "Expect method name.");
  uint8_t constant = identifierConstant(&parser.previous, false);
  FunctionType type = TYPE_METHOD;
  if (parser.previous.length == 4 &&
      memcmp(parser.previous.start, "init", 4) == 0) {
    type = TYPE_INITIALIZER;
  }

  function(type);
  emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expect class name.");
  Token className = parser.previous;
  uint8_t nameConstant = identifierConstant(&parser.previous, false);
  declareVariable(false);

  emitBytes(OP_CLASS, nameConstant);
  defineVariable(nameConstant);

  ClassCompiler classCompiler;
  classCompiler.enclosing = currentClass;
  currentClass = &classCompiler;

  namedVariable(className, false);
  consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    method();
  }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
  emitByte(OP_POP);

  currentClass = currentClass->enclosing;
}

static void funDeclaration() {
  uint8_t global = parseVariable("Expect function name.", false);
  markInitialized();
  function(TYPE_FUNCTION);
  defineVariable(global);
}

/*
 * Note: Constants have to be assigned immediately. Things like:
 * const i
 * if condition
 *    i = 0
 * else
 *    i = 1
 *
 * Are not allowed. (There is no control flow at this point in the language)
 */
static void constDeclaration() {

  uint8_t global = parseVariable("Expect variable name.", true);

  consume(TOKEN_EQUAL, "Constants have to be initialized after declaration.");
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  defineVariable(global);
}

static void varDeclaration() {
  // only saves the variable name to the constant array
  uint8_t global = parseVariable("Expect variable name.", false);

  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  defineVariable(global);
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(TOKEN_SEMICOLON)) {
    // No initializer.
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }

  int loopStart = currentChunk()->count;
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    // Jump out of the loop if the condition is false.
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  LoopInfo previousLoop = beginLoop(loopStart);

  statement();
  emitLoop(loopStart);

  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP);
  }
  endScope();
  exitLoop(previousLoop);
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();

  int elseJump = emitJump(OP_JUMP);

  patchJump(thenJump);
  emitByte(OP_POP);

  if (match(TOKEN_ELSE))
    statement();

  patchJump(elseJump);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    error("Can't return from top-level code.");
  }

  if (match(TOKEN_SEMICOLON)) {
    emitReturn();
  } else {
    if (current->type == TYPE_INITIALIZER) {
      error("Can't return a value from an initializer.");
    }

    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value");
    emitByte(OP_RETURN);
  }
}

static void whileStatement() {
  int loopStart = currentChunk()->count;

  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  // Store information for continue statements
  LoopInfo previousLoop = beginLoop(loopStart);

  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  emitLoop(loopStart);

  patchJump(exitJump);
  emitByte(OP_POP);
  exitLoop(previousLoop);
}

static void switchStatement() {
#define MAX_NUMBER_OF_CASES 128
  // There are two ways to compare the switch expression to each case
  // expression:
  // 1. We create an internal variable in which we store the value.
  //    The variable can be popped of automatically by creating a new scope for
  //    the switch case.
  //    This way, there is no change to the language necessary, but we need to
  //    read the variable for each case.
  // 2. We create a switch case comparison which leaves the switch expression on
  //    the stack. We pop it manually at the end.
  //    This way, we don't need to emit unnecessary read local opcodes.
  //
  // For this implementation, i went with option 2.
  int exitJumps[MAX_NUMBER_OF_CASES];
  int numberOfJumps = 0;

  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before switch cases.");

  while (match(TOKEN_SWITCH_CASE) && numberOfJumps < MAX_NUMBER_OF_CASES) {
    expression();
    consume(TOKEN_COLON, "Expect ':' after switch case expression.");
    emitByte(OP_SWITCH_COMPARE);
    int nextCase = emitJump(OP_JUMP_IF_FALSE);

    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF) &&
           !check(TOKEN_SWITCH_DEFAULT) && !check(TOKEN_SWITCH_CASE)) {
      statement();
    }
    // Pop the result of the comparison in the true branch
    emitByte(OP_POP);
    // Jump to the end of the switch case
    exitJumps[numberOfJumps++] = emitJump(OP_JUMP);

    // It is important to patch the jump before we pop the result of the switch
    // case condition, otherwise the pop would not get executed by the vm.
    patchJump(nextCase);
    // Pop the result of the comparison in the false branch
    emitByte(OP_POP);
  }

  if (numberOfJumps >= MAX_NUMBER_OF_CASES)
    error("Maximum number of cases is 128.");

  if (match(TOKEN_SWITCH_DEFAULT)) {
    consume(TOKEN_COLON, "Expect ':' after 'default'.");
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
      statement();
    }
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after switch statement.");

  for (int exitJumpIndex = 0; exitJumpIndex < numberOfJumps; exitJumpIndex++) {
    patchJump(exitJumps[exitJumpIndex]);
  }
  // Pop off the expression of the switch statement.
  emitByte(OP_POP);
#undef MAX_NUMBER_OF_CASES
}

static void continueStatement() {
  if (current->currentLoop.loopStart == NO_LOOP) {
    error("Expect continue statement only in 'for' or 'while' loops");
  }
  // Pop off all the locals in nested scopes.
  for (int localIndex = current->localCount - 1;
       current->locals[localIndex].depth > current->currentLoop.loopScope &&
       localIndex >= 0;
       localIndex--) {
    emitByte(OP_POP);
  }

  emitLoop(current->currentLoop.loopStart);

  consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'.");
}

static void synchronize() {
  parser.panicMode = false;

  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON)
      return;
    switch (parser.current.type) {
    case TOKEN_CLASS:
    case TOKEN_FUN:
    case TOKEN_VAR:
    case TOKEN_FOR:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_PRINT:
    case TOKEN_RETURN:
      return;
    default:; // Do nothing.
    }

    advance();
  }
}

static void declaration() {
  if (match(TOKEN_CLASS)) {
    classDeclaration();
  } else if (match(TOKEN_FUN)) {
    funDeclaration();
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else if (match(TOKEN_CONST)) {
    constDeclaration();
  } else {
    statement();
  }

  if (parser.panicMode)
    synchronize();
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_RETURN)) {
    returnStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else if (match(TOKEN_SWITCH)) {
    switchStatement();
  } else if (match(TOKEN_CONTINUE)) {
    continueStatement();
  } else {
    expressionStatement();
  }
}

static void grouping(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary(bool canAssign) {
  TokenType operatorType = parser.previous.type;

  // Compile the operand.
  parsePrecedence(PREC_UNARY);

  // Emit the operator instruction.
  switch (operatorType) {
  case TOKEN_BANG:
    emitByte(OP_NOT);
    break;
  case TOKEN_MINUS:
    emitByte(OP_NEGATE);
    break;
  default:
    return; // Unreachable
  }
}

static void number(bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void string(bool canAssign) {
  emitConstant(OBJ_VAL(
      constString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);
  bool isConstant;
  // fix this
  //
  if (arg != -1) {
    isConstant = current->locals[arg].isConstant;
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else if ((arg = resolveUpvalue(current, &name)) != -1) {
    getOp = OP_GET_UPVALUE;
    setOp = OP_SET_UPVALUE;
  } else {
    // Note: for global variables we now forbid a referencing it before it is
    // declared. Otherwise it is very difficult to determine whether a global
    // variable is const or not.
    Global global;
    if (getGlobal(&name, &global)) {
      isConstant = global.isConstant;
      arg = global.indexInCurrentChunkValues;
      getOp = OP_GET_GLOBAL;
      setOp = OP_SET_GLOBAL;
    } else {
      error("Can not refer to global before it is declared.");
      return;
    }
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    if (isConstant) {
      error("Can not assign constant");
      return;
    } else {
      expression();
      emitBytes(setOp, arg);
    }
  } else {
    emitBytes(getOp, arg);
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

static void this_(bool canAssign) {
  if (currentClass == NULL) {
    error("Can't use 'this' outside of a class.");
    return;
  }
  variable(false);
}

ObjFunction *compile(const char *source) {
  initScanner(source);
  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);

  if (!globals.isInitialized) {
    globals.globalCount = 0;
    initTable(&globals.globalVariableNames);
    globals.isInitialized = true;

    for (int i = 0; i < NUMBER_OF_NATIVES; i++) {
      Native *native = &natives[i];
      Token dummyToken = {
          .start = native->name,
          .type = TOKEN_FUN,
          .line = 0,
          .length = strlen(native->name),
      };

      addGlobal(&dummyToken, false);
    }
  }

  parser.hadError = false;
  parser.panicMode = false;

  advance();

  while (!match(TOKEN_EOF)) {
    declaration();
  }

  ObjFunction *function = endCompiler();
  return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
  Compiler *compiler = current;
  while (compiler != NULL) {
    markObject((Obj *)compiler->function);
    markTable(&compiler->variablesAtIndex);
    compiler = compiler->enclosing;
  }
}
