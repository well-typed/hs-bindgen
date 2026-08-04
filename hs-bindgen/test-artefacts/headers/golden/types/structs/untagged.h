// Struct containing an untagged inner struct
struct S1 {
  struct {
    int a;
    int b;
  } c;

  int d;
};

// Struct containing nested untagged structs
struct S2 {
  struct {
    int a;
    struct {
      int b;
    } deep;
  } inner;

  int d;
};

// Struct containing a _pointer to_ an untagged inner struct
struct S3 {
  struct {
    int a;
    int b;
  } **c;

  int d;
};
