#pragma once

// Implicit field detection works the same if any of the indirect fields are
// bit-fields.  Note that unnamed bit-field declarations, which specify padding,
// are not fields.
//
// Note: implicit field detection works the same for both unions and structs in
// any order of nesting, so testing the struct-only case should be sufficient to
// cover those examples as well.

// With S1 we test one level of nesting, with S2 we test two levels of nesting.

struct S1 {
  struct {
    char y : 3;
  };
  int x;
};

struct S2 {
  struct {
    struct {
      char y : 3;
    };
    int x;
  };
};
