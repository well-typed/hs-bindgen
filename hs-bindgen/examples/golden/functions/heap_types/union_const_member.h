// See issue #1490

union U {
  const int x;
};

typedef union U T;

T fun (T x);