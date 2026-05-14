// Simple version.

// Test that macros defined in outer header are visible in inner header which is
// included _after_ the macro was defined in the outer header. See issue
// https://github.com/well-typed/hs-bindgen/issues/1699.

#define OUTER_A 1

#include "simple_inner.h"
