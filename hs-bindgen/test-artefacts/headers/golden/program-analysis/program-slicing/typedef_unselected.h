#include "typedef_unselected_t.h"

// T (see typedef_unselected_t.h) is NOT selected as a transitive dependency of foo because
// program slicing is disabled
//
// A warning message should also be printed that T is not selected due to a
// missing transitive dependency on T
void foo(T x);

// We would expect that we generate bindings only for U and bar, not for T and foo
#define U int
void bar (U x);
