// bar parses successfully, but foo does not because long double is not
// supported. bar has file scope because it inherits the scope from foo, so we
// should still generate bindings for bar even if we don't generate bindings for
// foo.
//
// Info on scoping:
// <https://en.cppreference.com/w/c/language/scope.html#Notes>
//
// NOTE: hs-bindgen handles nesting the same regardless of whether using
// structs, unions, or any combination of them, in any order. So it should be
// sufficient to only test with structs.

struct foo {
  struct bar {
    //!
    //! Comment attached to foo::bar::x1_1.
    //!
    //! This comment must be preserved, even though the enclosing declaration
    //! uses an unsupported feature.
    //!
    int x1_1;
  } x1;
  long double x2;
};

extern struct bar X;
