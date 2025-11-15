// `clang-16` and up assign the name `foo` to the anonymous struct (older
// versions return an empty string), resulting in name clashes unless we squash
// that second struct, leaving only `struct foo` (the first struct) and `foo`
// (the typedef).
// See also https://github.com/well-typed/hs-bindgen/issues/1307.
struct foo { char a; char b; };
typedef struct { double x; double y; } foo;
