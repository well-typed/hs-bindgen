#pragma once

// x is an indirect field of the unnamed struct at field S.y, but it is not an
// indirect field of struct S.
//
// This edge case was not properly handled by implicit field detection in the
// past, so we include this as an edge case regression test.
//
// Note: implicit field detection works (nearly fully) the same for both unions
// and structs in any order of nesting. Testing the struct-only case should be
// sufficient to cover those examples as well.

struct S {
  struct {
    struct {
      int x;
    };
  } y;
};

