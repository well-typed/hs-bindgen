struct OkBefore {
  int x;
};

// `struct Fail` can not be translated because it contains a `long double`.
struct Fail {
  long double x;
  // We are not aware that `struct Fail` depends on `struct OkBefore`, but that
  // is not a problem, because we can not generate bindings for `struct Fail`
  // anyways.
  struct OkBefore y;
};

// Depend on `struct Fail` by value. We can not directly detect that a
// dependency is missing.
struct DependOnFailByValue {
  struct Fail x;
};

// Depend on `struct Fail` by reference. We can not directly detect that a
// dependency is missing.
struct DependOnFailByReference {
  struct Fail *x;
};

struct OkAfter {
  int x;
};
