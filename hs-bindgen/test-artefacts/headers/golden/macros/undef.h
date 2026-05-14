// TODO <https://github.com/well-typed/hs-bindgen/issues/1956>: generated
// bindings for undef-ed macros fail to compile
#define T int
void foo (T x);
#undef T
