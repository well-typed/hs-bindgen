// See issue #1490

struct S {
  const int x;
};

typedef struct S T;

T fun (T x);