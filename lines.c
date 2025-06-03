#include "common.h"
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

#include "lines.h"
#include "memory.h"

static bool doesExist(LinesEncoding *encoding, int line) {
  return encoding->count != 0 && encoding->lines[encoding->count - 1] == line;
}

void initLinesEncoding(LinesEncoding *encoding) {
  encoding->lineCounts = NULL;
  encoding->lines = NULL;
  encoding->count = 0;
  encoding->capacity = 0;
}

void writeLinesEncoding(LinesEncoding *encoding, int line) {
  if (doesExist(encoding, line)) {
    assert(encoding->lineCounts[encoding->count - 1] != INT_MAX);
    encoding->lineCounts[encoding->count - 1]++;
  } else {
    if (encoding->capacity < encoding->count + 1) {
      int oldCapacity = encoding->capacity;
      encoding->capacity = GROW_CAPACITY(oldCapacity);
      encoding->lines =
          GROW_ARRAY(int, encoding->lines, oldCapacity, encoding->capacity);
      encoding->lineCounts = GROW_ARRAY(int, encoding->lineCounts, oldCapacity,
                                        encoding->capacity);
    }

    encoding->lineCounts[encoding->count] = 1;
    encoding->lines[encoding->count] = line;
    encoding->count++;
  }
}

void freeLinesEncoding(LinesEncoding *encoding) {
  FREE_ARRAY(int, encoding->lineCounts, encoding->capacity);
  FREE_ARRAY(int, encoding->lines, encoding->capacity);
  initLinesEncoding(encoding);
}

int getLine(LinesEncoding *encoding, int count) {
  for (int i = 0; i < encoding->count; i++) {
    int currentLineCount = encoding->lineCounts[i];
    count -= currentLineCount;

    if (count < 0) {
      return encoding->lines[i];
    }
  }

  return -1;
}
