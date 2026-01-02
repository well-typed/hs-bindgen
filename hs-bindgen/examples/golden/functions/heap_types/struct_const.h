// See issue #1490

struct S {
  int x;
};

typedef struct S T;

const T fun (const T x);