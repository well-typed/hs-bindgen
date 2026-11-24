#ifndef CLASHES_WITH_UNUSABLE_UNUSABLE_H
#define CLASHES_WITH_UNUSABLE_UNUSABLE_H

// struct foo fails to parse, because long double is unsupported, so the
// library plan never places it. The typedef foo points at it under the same
// name, so name mangling asks whether the two share a module.
struct foo {
  long double x;
};

typedef struct foo *foo;

struct kept {
  int x;
};

#endif
