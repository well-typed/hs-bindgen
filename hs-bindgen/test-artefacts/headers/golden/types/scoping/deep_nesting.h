// Variant of nesting.h with recursive nesting, hence the name deep nesting.
//
// baz parses successfully, but foo and bar do not because long double is not
// supported. baz has file scope because it inherits the scope from bar and
// recursively from foo, so we should still generate bindings for baz even if we
// don't generate bindings for foo and bar.
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
    struct baz {
      int x1_2_1;
    } x1_2;
  } x1;
  long double x2;
};

extern struct baz X;

