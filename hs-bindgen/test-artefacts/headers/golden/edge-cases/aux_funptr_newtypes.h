// See issue #1694. Auxiliary newtypes for function pointer typedefs used to be
// processed before their dependencies. Fixed in PR #1724.

struct foo;

typedef void (*FunPtr)(struct foo x);

struct foo {
  int x;
  int y;
};
