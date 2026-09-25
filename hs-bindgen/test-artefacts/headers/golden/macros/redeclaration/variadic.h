// `A` is ambiguous (the `#undef` is invisible to hs-bindgen). In `ID`, `A` is a
// parameter, so the expansion of `ID` does not depend on the macro `A`.
#define A int
#undef A
#define A char
#define ID(A, ...) A __VA_ARGS__
typedef ID(const, int) T;
