#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table_generic.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

// Forward declarations
static uint32_t hashValue(const Value);

// Implementations
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

static inline uint32_t hashArray(const ValueArray *arr) {
  uint32_t hash = 2166136261u;

  for (int i = 0; i < arr->count; i++) {
    hash ^= hashValue(arr->values[i]);
    hash *= 16777619;
  }

  return hash;
}

static inline uint32_t hashObject(const Obj *obj) {
  switch (TYPE(obj)) {
  case OBJ_STRING:
    return ((ObjString *)obj)->hash;
  case OBJ_CONST_STRING: {
    ObjConstString *str = (ObjConstString *)obj;
    return hashString(str->chars, str->length);
  }
  case OBJ_ARRAY: {
    ObjArray *array = (ObjArray *)obj;
    return hashArray(&array->array);
  }
  }
}

static uint32_t hashValue(const Value key) {
  switch (key.type) {
  case VAL_BOOL:
    return hashBool(AS_BOOL(key));
  case VAL_NUMBER:
    return hashNumber(AS_NUMBER(key));
  case VAL_INTERNAL:
    return AS_INTERNAL(key); // No need to hash an integer
  case VAL_OBJ:
    return hashObject(AS_OBJ(key));
  case VAL_NIL:
    return 0;
  }
}

void initTable(TableGeneric *table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(TableGeneric *table) {
  FREE_ARRAY(EntryGeneric, table->entries, table->capacity);
  initTable(table);
}

static EntryGeneric *findEntry(EntryGeneric *entries, int capacity,
                               Value *key) {
  uint32_t index = hashValue(*key) % capacity;
  EntryGeneric *tombstone = NULL;

  for (;;) {
    EntryGeneric *entry = &entries[index];
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

bool tableGet(TableGeneric *table, Value *key, Value *value) {
  if (table->count == 0)
    return false;

  EntryGeneric *entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  *value = entry->value;
  return true;
}

static void adjustCapacity(TableGeneric *table, int capacity) {
  EntryGeneric *entries = ALLOCATE(EntryGeneric, capacity);
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  table->count = 0;
  for (int i = 0; i < table->capacity; i++) {
    EntryGeneric *entry = &table->entries[i];
    if (entry->key == NULL)
      continue;

    EntryGeneric *dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  FREE_ARRAY(EntryGeneric, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableSet(TableGeneric *table, Value *key, Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  EntryGeneric *entry = findEntry(table->entries, table->capacity, key);
  bool isNewKey = entry->key == NULL;
  if (isNewKey && IS_NIL(entry->value))
    table->count++;

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

bool tableDelete(TableGeneric *table, Value *key) {
  if (table->count == 0)
    return false;

  // Find the entry
  EntryGeneric *entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  // Place a tombstone in the entry.
  entry->key = NULL;
  entry->value = BOOL_VAL(true);
  return true;
}

void tableAddAll(TableGeneric *from, TableGeneric *to) {
  for (int i = 0; i < from->capacity; i++) {
    EntryGeneric *entry = &from->entries[i];
    if (entry->key != NULL) {
      tableSet(to, entry->key, entry->value);
    }
  }
}
