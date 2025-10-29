struct Baz {
  int x;
};

// `struct Foo` can not be translated because it contains a `long double`.
struct Foo {
  long double x;
  // We are not aware that `struct Foo` depends on `struct Baz`, but that is not
  // a problem, because we can not generate bindings for `struct Foo` anyways.
  struct Baz y;
};

// `Bar` depends on `struct Foo` by value. We can not directly detect that a
// dependency is missing.
struct BarByValue {
  struct Foo x;
};

// `Bar` depends on `struct Foo` by reference. We can not directly detect that a
// dependency is missing.
struct BarByReference {
  struct Foo *x;
};
