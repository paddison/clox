#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "object.h"
#include "scanner.h"
#include "table.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

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
} Local;

typedef struct {
  Token name;
  bool isConstant;
  InternalNum indexInChunkValues;
} Global;

typedef struct {
  // Locals
  Local locals[UINT16_COUNT];
  int localCount;
  int scopeDepth;
  Table variablesAtIndex;
  // Globals
  Global globals[UINT8_COUNT];
  int globalCount;
  Table globalVariableNames;
} Compiler;

// forward declarations
static void unary(bool canAssign);
static void binary(bool canAssign);
static void number(bool canAssign);
static void string(bool canAssign);
static void variable(bool canAssign);
static void literal(bool canAssign);
static void expression();
static void grouping(bool canAssign);
static void statement();
static void declaration();

// clang-format off
ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
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
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};
// clang-format on

Parser parser;
Compiler *current = NULL;
Chunk *compilingChunk;

static Chunk *currentChunk() { return compilingChunk; }

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

static void emitReturn() {
  printf("emit return\n");
  emitByte(OP_RETURN);
}

static InternalNum makeConstant(Value value) {
  return writeConstant(currentChunk(), value, parser.previous.line);
}

static void emitConstant(Value value) { makeConstant(value); }

static void initCompiler(Compiler *compiler) {
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  compiler->globalCount = 0;
  initTable(&compiler->variablesAtIndex);
  initTable(&compiler->globalVariableNames);
  current = compiler;
}

static void endCompiler() {
  emitReturn();
  freeTable(&current->variablesAtIndex);
  freeTable(&current->globalVariableNames);
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), "code");
  }
#endif
}

static void beginScope() { current->scopeDepth++; }

static void endScope() {
  current->scopeDepth--;

  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
    emitByte(OP_POP);
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
  if (tableGet(&current->globalVariableNames, variableName, &index)) {
    *global = current->globals[AS_INTERNAL(index)];
    wasFound = true;
  }

  return wasFound;
}

static InternalNum addGlobal(Token *name, bool isConstant) {
  ObjString *variableName = copyString(name->start, name->length);
  Value variableNameAsValue = OBJ_VAL(variableName);
  InternalNum indexRaw = makeConstant(variableNameAsValue);
  Global *global = &current->globals[current->globalCount++];

  global->name = *name;
  global->indexInChunkValues = indexRaw;
  global->isConstant = isConstant;

  tableSet(&current->globalVariableNames, variableName,
           INTERNAL_VAL(current->globalCount - 1));

  return indexRaw;
}

// Returns where it is in the constantArray
static InternalNum identifierConstant(Token *name, bool isConstant) {
  Value index;
  Global *global;

  if (getGlobal(name, global)) {
    return global->indexInChunkValues;
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

static bool lookupIndexOfLocal(Token name, InternalNum *index) {
  ObjString *string = copyString(name.start, name.length);
  Value indicesOfLocalValue;
  bool isKnown = false;
  if (tableGet(&current->variablesAtIndex, string, &indicesOfLocalValue)) {
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

  if (lookupIndexOfLocal(*name, &index)) {
    Local *local = &compiler->locals[index];
    if (local->depth == -1) {
      error("Can't read local variable in its own initializer.");
    }

    foundIndex = (int)index;
  }

  return foundIndex;
}

static void addLocalToTable(Token name) {
  ObjString *string = copyString(name.start, name.length);
  Value indicesOfLocal;
  Value index = INTERNAL_VAL(current->localCount);

  if (!tableGet(&current->variablesAtIndex, string, &indicesOfLocal)) {
    indicesOfLocal = OBJ_VAL(allocateEmptyArray());
    tableSet(&current->variablesAtIndex, string, indicesOfLocal);
  }

  if (!IS_ARRAY(indicesOfLocal)) {
    fatal("Internal compiler error during variable lookup.");
  }

  writeValueArray(&AS_ARRAY(indicesOfLocal)->array, index);
}

static void addLocal(Token name, bool isConstant) {
  if (current->localCount == UINT16_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  addLocalToTable(name);

  Local *local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
  local->isConstant = isConstant;
  printf("Local %.*s at depth: %d, localCount: %d\n", name.length, name.start,
         current->scopeDepth, current->localCount);
}

static void declareVariable(bool isConstant) {
  if (current->scopeDepth == 0)
    return;

  Token *name = &parser.previous;

  InternalNum index;

  if (lookupIndexOfLocal(*name, &index)) {
    Local *local = &current->locals[index];

    if (local->depth == current->scopeDepth &&
        identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }

  addLocal(*name, isConstant);
}

static uint8_t parseVariable(const char *errorMessage, bool isConstant) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable(isConstant);
  if (current->scopeDepth > 0)
    return 0;

  return identifierConstant(&parser.previous, isConstant);
}

static void markInitialized() {
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
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

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
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
  if (match(TOKEN_VAR)) {
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
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
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
  } else {
    // Note: for global variables we now forbid a referencing it before it is
    // declared. Otherwise it is very difficult to determine whether a global
    // variable is const or not.
    Global global;
    if (getGlobal(&name, &global)) {
      isConstant = global.isConstant;
      arg = global.indexInChunkValues;
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

bool compile(const char *source, Chunk *chunk) {
  initScanner(source);
  Compiler compiler;
  initCompiler(&compiler);
  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  advance();

  while (!match(TOKEN_EOF)) {
    declaration();
  }

  endCompiler();
  return !parser.hadError;
}
