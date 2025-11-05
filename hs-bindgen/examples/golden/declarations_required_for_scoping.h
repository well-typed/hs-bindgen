#include <stddef.h>

// We expect that parsing the macro succeeds because `size_t` is a
// `typedef` which we always parse.
#define A size_t

void f(A x);
