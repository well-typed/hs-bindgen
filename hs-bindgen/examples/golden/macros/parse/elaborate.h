// Elaborate version.

// Test that macros defined in outer header are visible in inner header which is
// included _after_ the macro was defined in the outer header. See issue
// https://github.com/well-typed/hs-bindgen/issues/1699.

#define OUTER_A 1
typedef int outer_int;

#include "elaborate_inner.h"

#define OUTER_B INNER_A
#define OUTER_C INNER_B
