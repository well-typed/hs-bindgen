// Struct with implicit fields
//
// The order is relevant in the case that we compute offsets (as opposed to ask
// clang for offsets) if we flatten the struct.
struct has_implicit_fields {
  int x1;
  struct {
    int x2_1;
    int x2_2;
  }; // implicit field x2
  int x3;
  struct {
    int x4_1;
    int x4_2;
  }; // implicit field x4
  union {
    int x5_1;
    int x5_2;
  }; // implicit field x5
  int x5;
};

