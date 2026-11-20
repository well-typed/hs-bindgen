/* Function types in flexible array members */

typedef int T;

// The element type gets instances: T is defined in this module
struct S {
  int n;
  void (*cbs[])(T);
};
