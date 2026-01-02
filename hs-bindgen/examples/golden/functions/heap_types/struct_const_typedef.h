// See issue #1490

struct S {
  int x;
};

typedef const struct S T;

T fun (T x);