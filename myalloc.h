#include <stdint.h>
#include <stddef.h>

static const uint8_t HEAP[8192] = { 0 };

static size_t size = 0;

# define PGSIZE 4096;
// Taken from https://github.com/mit-pdos/xv6-public/blob/master/mmu.h#L90
# define PGROUNDUP(size) (((size) + PGSIZE - 1) & ~(PGSIZE - 1))

// always allocate at least 4k per request
static uint8_t requestHeapSpace(size_t requestedSize) {

}
