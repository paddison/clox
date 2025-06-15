#include "myalloc.h"
#include "assert.h"

/******************************************************************************
 * Preprocessor defines
 *****************************************************************************/

/**
 * 4 megabyte of memory
 */
#define MEMORYSIZE (4096 * 1000)
/**
 * One "page" is 4k
 */
#define PGSIZE 4096
/**
 * Rounds size up to be aligned with 4096.
 * Taken from https://github.com/mit-pdos/xv6-public/blob/master/mmu.h#L90
 */
#define PGROUNDUP(size) (((size) + PGSIZE - 1) & ~(PGSIZE - 1))

/******************************************************************************
 * local data structures
 *****************************************************************************/
typedef uint64_t Align;

static uint8_t HEAP[MEMORYSIZE] = {0};

static size_t size = 0;

typedef union header {
  struct {
    union header *next;
    uint32_t size;
  } h;
  Align x;
} Header;

/**
 * Initial element of the freeList.
 */
static Header base = {.h = {
                          .next = &base,
                          .size = 0,
                      }};
static Header *freeList = &base;

/******************************************************************************
 * local function declarations
 *****************************************************************************/

/**
 * \brief Requests new space from the HEAP.
 *        Rounds up requestedSize to be aligned with 4096.
 *
 * \param[in/out] requestedSize  The requested allocation size.
 *                               Gets rounded to be aligned to 4096.
 * \param[out]    addr           Stores the address of the newly allocated
 *                               Block of memory.
 *
 * \return AllocErr
 *           - ok, if the allocation was succesfull
 *           - outOfMemory, there is no more space left
 */
static AllocErr requestHeapSpace(size_t *requestedSize, uint8_t *addr);

/******************************************************************************
 * local function implementations
 *****************************************************************************/

static AllocErr requestHeapSpace(size_t *requestedSize, uint8_t *addr) {
  AllocErr retVal = outOfMemory;

  /* There is enough space to allocate */
  if (size + *requestedSize < MEMORYSIZE) {
    /* round up the requested size, so it is aligned to 4k */
    *requestedSize = PGROUNDUP(*requestedSize);
    addr = HEAP + size;

    size += *requestedSize;
    retVal = ok;
  }

  return retVal;
}

/******************************************************************************
 * global function implementations
 *****************************************************************************/

void *myMalloc(size_t size, size_t sizeOfType) {
  assert(PGSIZE / sizeof(Header) == 0);
  void *addr = NULL;
  const size_t sizeInBytes = size * sizeOfType + sizeof(Header);
  const size_t maxIter = MEMORYSIZE;
  Header *current = freeList;

  for (size_t i = 0; i < maxIter; i++) {

    /* Looped around freeList -> allocate more memory */
    if (current == freeList) {
      size_t availableSize = sizeInBytes;
      Header *chunk = NULL;
      if (requestHeapSpace(&availableSize, (uint8_t *)chunk) == outOfMemory) {
      }
    }
  }

  return addr;
}
