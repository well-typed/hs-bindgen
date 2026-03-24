#pragma once

// Anonymous structs and unions were added to C11. Since C11, the behavior of
// the program is undefined if a struct or union is defined without any named
// members (including those obtained via anonymous nested structs or unions).
// From C2y onwards, the behaviour will probably be implementation-defined.
//
// <https://en.cppreference.com/w/c/language/struct.html>
// <https://en.cppreference.com/w/c/language/union.html>
//
// For implicit field detection to work correctly, it is important that there
// are no empty anonymous nested structs or unions. We need at least one named
// member in an anonymous struct or union to determine the offset of that
// anonymous object within an enclosing object. So, the parser should not
// generate any bindings for empty structs or unions, and emit a message
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
  struct {};
  int x;
};

struct S2 {
  struct {
    struct {};
    int x;
  };
};
