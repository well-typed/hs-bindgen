#pragma once

// For implicit field detection to work correctly, it is important that there
// are no unnamed bit-fields in anonymous nested structs or unions. We can not
// ask for the offset of an unnamed bit-field, and we need that offset to
// compute implicit fields. So, the parser should not generate any bindings for
// a nested anonymous struct/union with an unnamed bit-field, and emit a message
// instead. Moreover, if a nested struct or union fails to parse, for example
// because it is empty, then the enclosing struct or union should also fail to
// parse.
//
// This edge case was not properly handled by implicit field detection in the
// past, so we include this as an edge case regression test.
//
// Note: implicit field detection works the same for both unions and structs in
// any order of nesting, so testing the struct-only case should be sufficient to
// cover those examples as well.

// With S1 we test one level of nesting, with S2 we test two levels of nesting.

struct S1 {
  struct {
    char : 3;
  };
  int x;
};

struct S2 {
  struct {
    struct {
      char : 3;
    };
    int x;
  };
};
