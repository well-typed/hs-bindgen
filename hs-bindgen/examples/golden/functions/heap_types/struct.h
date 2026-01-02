// See issue #1490

struct S {
  int x;
};

typedef struct S T;

T fun (T x);