// Multiple uses of anon decl by declaring multiple struct fields
// https://github.com/well-typed/hs-bindgen/issues/1418

struct some_struct {
  struct { int x; int y; } field1, field2, field3;
};
