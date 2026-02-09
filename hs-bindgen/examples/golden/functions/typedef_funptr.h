// Regression test for issue #1650: avoid duplicate FromFunPtr/ToFunPtr instances
//
// When a typedef function pointer is used in a struct field, only the _Aux
// newtype instance should be generated, not a duplicate instance for the bare
// function type.

// Typedef function pointer used in a struct field.
// Should generate: FromFunPtr/ToFunPtr for RunDriver_Aux only.
// Should NOT generate: FromFunPtr/ToFunPtr for (Ptr Driver -> IO CInt).
//
struct Driver;
typedef int (*RunDriver)(struct Driver *self);
struct Driver {
  RunDriver run;
};

// Bare function pointer in a struct field (no typedef).
// Should generate: FromFunPtr/ToFunPtr for the bare function type.
//
struct Bare {
  void (*callback)(int x);
};
