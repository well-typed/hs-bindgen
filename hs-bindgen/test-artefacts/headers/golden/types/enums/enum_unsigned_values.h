#include <stdint.h>

// Unsigned enum with values crossing the signed boundary
typedef enum : uint8_t {
  U8_ZERO  = 0,
  U8_127   = 127,
  U8_128   = 128,
  U8_255   = 255
} uint8_enum;
