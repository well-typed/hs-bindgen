// See issue #1490

union U {
  int x;
};

typedef union U T;

const T fun (const T x);