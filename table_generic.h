#ifndef clox_table_generic_h
#define clox_table_generic_h

#include "common.h"
#include "value.h"

typedef struct {
  Value *key;
  Value value;
} EntryGeneric;

typedef struct {
  int count;
  int capacity;
  EntryGeneric *entries;
} TableGeneric;

void initTable(TableGeneric *table);
void freeTable(TableGeneric *table);
bool tableGet(TableGeneric *table, Value *key, Value *value);
bool tableSet(TableGeneric *table, Value *key, Value value);
bool tableDelete(TableGeneric *table, Value *key);
void tableAddAll(TableGeneric *from, TableGeneric *to);

#endif
