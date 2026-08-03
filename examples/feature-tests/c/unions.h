#pragma once

#include <stdint.h>

/* -------------------------------------------------------------------------- */
/* setUnionPayload should not zero out unrelated bytes */

// See <https://github.com/well-typed/hs-bindgen/issues/2183>

typedef union {
  uint64_t x; // == 64 bits
  uint8_t y;  // == 8 bits
} U;

uint64_t get_u_x (U * u);
void     set_u_x (U * u, uint64_t x);
_Bool    eq_u_x  (U * u1, U* u2);

uint8_t  get_u_y (U * u);
void     set_u_y (U * u, uint8_t y);
_Bool    eq_u_y  (U * u1, U* u2);
