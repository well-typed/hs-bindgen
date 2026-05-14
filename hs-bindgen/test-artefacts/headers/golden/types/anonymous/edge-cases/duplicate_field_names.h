#pragma once

// x occurs as a field name in S, but also in the unnamed struct at field S.y.
// They are still separate fields, regardless of the duplicate name.
//
// This edge case was not properly handled by implicit field detection in the
// past, so we include this as an edge case regression test.
//
// Note: implicit field detection works the same for both unions and structs in
// any order of nesting, so testing the struct-only case should be sufficient to
// cover those examples as well.

struct S {
  unsigned long long x;
  struct {
    struct {
      int x;
    };
  } y;
};

