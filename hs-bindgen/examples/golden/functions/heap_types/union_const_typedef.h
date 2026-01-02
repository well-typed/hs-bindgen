// See issue #1490

union U {
  int x;
};

typedef const union U T;

T fun (T x);