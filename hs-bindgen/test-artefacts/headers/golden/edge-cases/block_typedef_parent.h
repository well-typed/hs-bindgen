// The definition of S, in the child header, comes before B in source order,
// yet S has a field of type B; B must be generated first.
// https://github.com/well-typed/hs-bindgen/issues/2395

struct S;
typedef void (^B)(struct S);

#include "block_typedef_child.h"
