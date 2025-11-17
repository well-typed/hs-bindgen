// Test that we correctly detect type extension of newtype fields. In this case,
// we need `DataKinds` for the type literal `3` of constant arrays.
typedef int triplet[3];
