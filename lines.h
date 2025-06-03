#ifndef clox_lines_h
#define clox_lines_h

typedef struct {
  int count;
  int capacity;
  int *lines;
  int *lineCounts;
} LinesEncoding;

void initLinesEncoding(LinesEncoding *encoding);
void writeLinesEncoding(LinesEncoding *encoding, int line);
void freeLinesEncoding(LinesEncoding *encoding);
int getLine(LinesEncoding *encoding, int count);

#endif
