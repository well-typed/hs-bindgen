#include "_internal_macros.h"

// TODO <https://github.com/well-typed/hs-bindgen/issues/2210>:
// replace the code below with:
/*
MkMultiNestedS(enum { a } x)
*/

struct S {
  struct {
    struct {
      enum { a } x;
    };
  };
};
