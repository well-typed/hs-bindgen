// Struct containing an anonymous inner struct
struct S1 {
  struct {
    int a;
    int b;
  } c;

  int d;
};

// Struct containing nexted anonymous structs
struct S2 {
  struct {
    int a;
    struct {
      int b;
    } deep;
  } inner;

  int d;
};

// Struct containing a _pointer to_ an anonymous inner struct
struct S3 {
  struct {
    int a;
    int b;
  } **c;

  int d;
};
