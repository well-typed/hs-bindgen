// Variant of nesting.h with multiple nested structs side by side, hence the
// name wide nesting.
//
// baz parses successfully, but foo and bar do not because long double is not
// supported. baz has file scope because it inherits the scope from foo, so we
// should still generate bindings for baz even if we don't generate bindings for
// foo and bar.
//
// Info on scoping:
// <https://en.cppreference.com/w/c/language/scope.html#Notes>
//
// NOTE: hs-bindgen handles nesting the same regardless of whether using
// structs, unions, or any combination of them, in any order. So it should be
// sufficient to only test with structs.

struct foo {
  struct bar {
    long double x1_1;
  } x1;
  long double x2;
  struct baz {
    int x3_1;
  } x3;
};

extern struct baz X;

