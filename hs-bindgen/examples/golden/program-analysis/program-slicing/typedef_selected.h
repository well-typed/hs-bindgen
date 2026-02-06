#include "typedef_selected_t.h"

// T (see typedef_selected_t.h) is selected as a transitive dependency of foo
// with program slicing enabled
void foo(T x);

// We would expect that we generate bindings for foo and T in the same way as
// generate them for U and bar
#define U int
void bar (U x);
