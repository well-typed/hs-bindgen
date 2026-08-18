#include "_internal_macros.h"

// TODO <https://github.com/well-typed/hs-bindgen/issues/2210>:
// replace the code below with:
/*
MkSingleNestedS(union { int a; } x)
*/

struct S {
  struct {
    union {
      int a;
    } x;
  };
};
