#include "unions.h"

/* -------------------------------------------------------------------------- */
/* setUnionPayload should not zero out unrelated bytes */

// See <https://github.com/well-typed/hs-bindgen/issues/2183>

uint64_t get_u_x (U * u)             { return u->x; };
void     set_u_x (U * u, uint64_t x) { u->x = x; };
_Bool    eq_u_x  (U * u1, U* u2)     { return u1->x == u2->x; };

uint8_t  get_u_y (U * u)             { return u->y; };
void     set_u_y (U * u, uint8_t y)  { u->y = y; };
_Bool    eq_u_y  (U * u1, U* u2)     { return u1->y == u2->y; };
