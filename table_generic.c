#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table_generic.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

static inline uint32_t hashBool(const bool boolean) { return boolean ? 1 : 0; }

static inline uint32_t hashNumber(const double number) {
  if (number == -0.0 || number == 0.0) {
    // Case 1: -0 and 0 return the same hash
    return 0;
  } else if (number != number) {
    // Case 2: All NaN numebers return the same hash
    // using an arbitrary large prime number.
    return 2147483647;
  } else {
    // Case 3: Get the bits of the double number and perform a simple hash
    uint64_t number_as_int = *((uint32_t *)&number);
    return (uint32_t)(number_as_int ^ (number_as_int >> 31));
  }
}

static uint32_t hashString(const char *key, int length) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

static inline uint32_t hashObject(const Obj *obj) {
  switch (obj->type) {
  case OBJ_STRING:
    return ((ObjString *)obj)->hash;
  case OBJ_CONST_STRING: {
    ObjConstString *str = ((ObjConstString *)obj);
    return hashString(str->chars, str->length);
  }
  }
}

static uint32_t hashValue(const Value key) {
  switch (key.type) {
  case VAL_BOOL:
    return hashBool(AS_BOOL(key));
  case VAL_NUMBER:
    return hashNumber(AS_NUMBER(key));
  case VAL_OBJ:
    return hashObject(AS_OBJ(key));
  case VAL_NIL:
    return 0;
  }
}

void initTable(Table *table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table *table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

static Entry *findEntry(Entry *entries, int capacity, Value *key) {
  uint32_t index = hashValue(*key) % capacity;
  Entry *tombstone = NULL;

  for (;;) {
    Entry *entry = &entries[index];
    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // Empty entry.
        return tombstone != NULL ? tombstone : entry;
      } else {
        // We found a tombstone.
        if (tombstone == NULL)
          tombstone = entry;
      }
    } else if (valuesEqual(*entry->key, *key)) {
      return entry;
    }

    index = (index + 1) % capacity;
  }
}

bool tableGet(Table *table, Value *key, Value *value) {
  if (table->count == 0)
    return false;

  Entry *entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  *value = entry->value;
  return true;
}

static void adjustCapacity(Table *table, int capacity) {
  Entry *entries = ALLOCATE(Entry, capacity);
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  table->count = 0;
  for (int i = 0; i < table->capacity; i++) {
    Entry *entry = &table->entries[i];
    if (entry->key == NULL)
      continue;

    Entry *dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableSet(Table *table, Value *key, Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  Entry *entry = findEntry(table->entries, table->capacity, key);
  bool isNewKey = entry->key == NULL;
  if (isNewKey && IS_NIL(entry->value))
    table->count++;

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

bool tableDelete(Table *table, Value *key) {
  if (table->count == 0)
    return false;

  // Find the entry
  Entry *entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  // Place a tombstone in the entry.
  entry->key = NULL;
  entry->value = BOOL_VAL(true);
  return true;
}

void tableAddAll(Table *from, Table *to) {
  for (int i = 0; i < from->capacity; i++) {
    Entry *entry = &from->entries[i];
    if (entry->key != NULL) {
      tableSet(to, entry->key, entry->value);
    }
  }
}
