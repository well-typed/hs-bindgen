/* Cross-declaration collision: the C identifiers `y` and `Y` lead to a single
   colliding Haskell identifier `Y`. */
union y;
union Y {
  int m;
  int o;
};

/* Intra-declaration collision: the enum tag `Color` and its enumerator `Color`
   both yield the Haskell name `Color` in the data-constructor namespace (the
   newtype data constructor and the enumerator's pattern synonym). */
enum Color { Color };

/* Intra-declaration collision via case mangling: the enum tag `A` yields the
   newtype data constructor `A`, while the lowercase enumerator `a` is mangled up
   to the pattern synonym `A`, so both collide in the data-constructor namespace.
   See https://github.com/well-typed/hs-bindgen/issues/2020. */
enum A { a };

/* Cross-declaration field/function collision: the struct field selector `s_x`
   (from `struct S`) collides with the function `S_x` (also mangled to `s_x`). */
struct S { int x; };
void S_x(void);
