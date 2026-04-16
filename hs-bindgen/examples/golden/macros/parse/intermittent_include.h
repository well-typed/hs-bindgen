// Test that we sort macros into sequence order even with intermittent includes.

// (At the moment, we do not perform any output sorting, and the generated
// bindings are in dependency order, with inner includes sorted before other
// declarations).

#define M1 T1

typedef int T1;

#define M2 T1

#include "intermittent_include_inner.h"

#define M3 T2

typedef int T2;

#define M4 T2
