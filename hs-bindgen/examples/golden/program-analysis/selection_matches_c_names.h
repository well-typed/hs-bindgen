// Specifically  test that  select predicates  match  against C  names, and  not
// against Haskell names  assigned by a prescriptive  binding specification. See
// https://github.com/well-typed/hs-bindgen/issues/1518.

int FunctionWithAssignedHaskellNameByNameMangler();

struct StructWithAssignedHaskellNameByPrescriptiveBindingSpecs {
  int x;
};
