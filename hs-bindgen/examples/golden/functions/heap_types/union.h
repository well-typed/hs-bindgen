// See issue #1490

union U {
  int x;
};

typedef union U T;

T fun (T x);