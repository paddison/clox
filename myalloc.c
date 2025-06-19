#include "myalloc.h"
#include "assert.h"
#include "stdbool.h"
#include "string.h"

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
#define UNITSIZE(size, width) (((size) + (width) - 1) / (width) + 1)

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

typedef enum AllocErr {
  ok,
  outOfMemory,
} AllocErr;


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
static AllocErr requestHeapSpace(size_t *requestedSize, uint8_t **addr);

/**
 * \brief Growths the freelist by appending a new block of at least required 
 *        size to it.
 *        Calls requestHeapSpace internally to obtain a new block.
 *
 * \param[in]     requiredSize  The minimum size required of the new block.
 * \param[in/out] current       The element of the freelist on which to 
 *                              append the block to.
 *
 * \return AllocErr
 *           - ok, the block was appended succesfully
 *           - outOfMemory, no memory left block couldn't be appended.
 *
 */
static AllocErr growFreeList(const size_t requiredSize, Header *current);

/******************************************************************************
 * local function implementations
 *****************************************************************************/

static AllocErr requestHeapSpace(size_t *requestedSize, uint8_t **addr) {
  AllocErr retVal = outOfMemory;

  /* There is enough space to allocate */
  if (size + *requestedSize < MEMORYSIZE) {
    /* round up the requested size, so it is aligned to 4k */
    *requestedSize = PGROUNDUP(*requestedSize);
    *addr = HEAP + size;

    size += *requestedSize;
    retVal = ok;
  }

  return retVal;
}

static AllocErr growFreeList(const size_t requiredSize, Header *current) {
  size_t availableSize = requiredSize * sizeof(Header);
  Header *chunk = NULL;
  AllocErr isMemoryAvailable = requestHeapSpace(
      &availableSize, 
      (uint8_t**)&chunk);

  if (isMemoryAvailable == ok) {
    assert(availableSize % sizeof(Header) == 0);
    availableSize /= sizeof(Header);
    assert(availableSize >= requiredSize);
    /* addresses returned by requestHeapSpace are guaranteed to be higher */
    chunk->h.next = freeList;
    chunk->h.size = availableSize;
    current->h.next = chunk;
  } 

  return isMemoryAvailable;
}

/******************************************************************************
 * global function implementations
 *****************************************************************************/

void *myMalloc(size_t size, size_t sizeOfType) {
  assert(PGSIZE % sizeof(Header) == 0);
  Header *addr = NULL;
  /* the required size needed to make an allocation of "size". */
  /* is padded to be aligned to header and includes space for the header itself */
  const size_t requiredSize = UNITSIZE(size * sizeOfType, sizeof(Header));
  const size_t maxIter = MEMORYSIZE;
  Header *current = freeList;

  for (size_t i = 0; i < maxIter; i++) {
    /* Found an chunk which is large enough */
    if (current->h.size >= requiredSize) {
      /* return tail end of the chunk. */
      const size_t remainingSize = current->h.size - requiredSize;
      addr = current + remainingSize; 
      addr->h.size = requiredSize;
      addr += 1;
      if (remainingSize > 0) {
        current->h.size = remainingSize;
      }
      freeList = current; /* set start of list to free chunk */
      break;
    }

    /* Looped around freeList -> allocate more memory */
    if (current->h.next == freeList) {
      if (growFreeList(requiredSize, current) == outOfMemory) {
        break;
      }
    }

    /* go to next element in list */
    current = current->h.next;
  }

  return addr;
}

/* todo: clean this up */
void myFree(void* memory) {
  Header* chunkToFree = ((Header*) memory) - 1;
  const size_t maxIter = MEMORYSIZE;
  Header* current = freeList;

  for (size_t i = 0; i < maxIter; i++) {
    if (chunkToFree > current && chunkToFree < current->h.next) {
      /* put chunk back someplace inside the memory area */
      /* merge with right memory area */
      /* | ... | current | chunkToFree M current->next | ... | */
      if (current->h.next == chunkToFree + chunkToFree->h.size) {
        chunkToFree->h.size += current->h.next->h.size;
        chunkToFree->h.next = current->h.next->h.next;
      } else {
        chunkToFree->h.next = current->h.next; 
      }
      /* merge with left memory area */
      /* | ... | current M chunkToFree | current->next | ... | */
      if (current + current->h.size == chunkToFree) {
        current->h.size += chunkToFree->h.size;
        current->h.next = chunkToFree->h.next;
      } else {
        current->h.next = chunkToFree;
      }
      break;
    } else if (current->h.next < current && chunkToFree > current) {
      /* put chunk to end of memory area */
      /* merge with current (left of chunk to free) */
      /* | current->next | ... | current M chunkToFree | */
      if (current + current->h.size == chunkToFree) {
        current->h.size += chunkToFree->h.size;
      } else {
        chunkToFree->h.next = current->h.next; 
        current->h.next = chunkToFree;
      }
      break;
    } else if (current->h.next < current && chunkToFree < current->h.next) {
      /* put chunk to start of memory area */
      /* merge with current->next (right of chunk to free) */
      /* | chunkToFree M current->next | ... | current | */
      if (current->h.next == chunkToFree + chunkToFree->h.size) {
        chunkToFree->h.size += current->h.next->h.size;
        chunkToFree->h.next = current->h.next->h.next;
        current->h.next = chunkToFree;
      } else {
        chunkToFree->h.next = current->h.next; 
        current->h.next = chunkToFree;
      }
      break;
    } else {
      /* go to next element in list */
      current = current->h.next;
    }
  }
}

/*
 * Manpage of realloc:
 *
 * ============================================================================
 *
 * The  realloc()  function changes the size of the memory block pointed to by 
 * ptr to size bytes.  
 * The contents of the memory will be unchanged in the range from the start of 
 * the region up to the minimum of the old and new sizes.  
 * If the new size  is  larger than the old size, the added memory will not be 
 * initialized.
 *
 * If ptr is NULL, then the call is equivalent to malloc(size), for all values 
 * of size.
 *
 * If  size  is  equal  to zero, and ptr is not NULL, then the call is 
 * equivalent to free(ptr) (but see "Nonportable behavior" for portability 
 * issues).
 *
 * Unless ptr is NULL, it must have been returned by an earlier call to malloc 
 * or related functions.  If the area pointed  to  was moved, a free(ptr) is 
 * done.
*/

void *myRealloc(void *oldPtr, size_t size, size_t sizeOfType) {
  void* retVal = NULL;
 /* If ptr is NULL, then the call is equivalent to malloc(size), for all values 
    of size. */
  if (oldPtr == NULL) {
    retVal = myMalloc(size, sizeOfType);
 /* If  size  is  equal  to zero, and ptr is not NULL, then the call is 
  equivalent to free(ptr) */
  } else if (size == 0) {
    assert(retVal == NULL);
  } else {
    Header* header = ((Header*) oldPtr) - 1;
    uint32_t currentSize = header->h.size;
    uint32_t requiredSize = UNITSIZE(size * sizeOfType, sizeof(Header));

    /* These could be optimized some more by implementing them manually,
     * however, for testability it is easier to use the existing functions */
    /* Case 1: Grow memory */
    if (currentSize < requiredSize) {
      retVal = myMalloc(size, sizeOfType);
      /* subtract 1 from current size, because we don't want to copy the header */
      memmove(retVal, oldPtr, (currentSize - 1) * sizeof(Header));
      myFree(oldPtr);
      /* Case 2: Shrink memory */
    } else if (currentSize > requiredSize) {
      retVal = myMalloc(size, sizeOfType);
      memmove(retVal, oldPtr, (size * sizeOfType));
      myFree(oldPtr);
    } else {
      /* Case 3: memory stays the same, Noop */
      retVal = oldPtr;
    }
  }

  return retVal;
}
