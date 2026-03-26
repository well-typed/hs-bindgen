#pragma once

// We can compute offsets for implicit fields even if the first field(s) of an
// anonymous struct or union are unnamed bit-fields (that introduce padding).
//
// This edge case was not properly handled by implicit field detection in the
// past, so we include this as an edge case regression test.

// Naming: the types in this header are named after the order in which objects
// are nested. For example, US means "union containing struct", or equivalently,
// "struct in union".

struct SS {
  char x;
  struct {
    char : 3;
    int y;
  };
  int z;
};

struct SU {
  char x;
  union {
    char : 3;
    int y;
  };
  int z;
};

union US {
  char x;
  struct {
    char : 3;
    int y;
  };
  int z;
};

union UU {
  char x;
  union {
    char : 3;
    int y;
  };
  int z;
};
