#include "_internal_macros.h"

// TODO <https://github.com/well-typed/hs-bindgen/issues/2210>:
// replace the code below with:
/*
MkSingleNestedS(union T { int a; } x)
*/

struct S {
  struct {
    union T {
      int a;
    } x;
  };
};
