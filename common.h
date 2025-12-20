#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION

#define DEBUG_STRESS_GC
#define DEBUG_LOG_GC

#define USE_CUSTOM_MALLOC

#define UINT8_COUNT (UINT8_MAX + 1)
#define UINT16_COUNT (UINT16_MAX + 1)

typedef uint16_t InternalNum;
#define INTERNAL_MAX UINT16_MAX

#undef DEBUG_STRESS_GC
#undef DEBUG_LOG_GC
#undef USE_CUSTOM_MALLOC

#endif
