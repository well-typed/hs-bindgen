#include "_internal_macros.h"

// TODO <https://github.com/well-typed/hs-bindgen/issues/2210>:
// replace the code below with:
/*
MkMultiNestedS(enum T { a } x)
*/

struct S {
  struct {
    struct {
      enum T { a } x;
    };
  };
};
