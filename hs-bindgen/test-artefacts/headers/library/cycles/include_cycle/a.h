#ifndef INCLUDE_CYCLE_A_H
#define INCLUDE_CYCLE_A_H

// The first declaration of struct b is this forward declaration. It does not
// move struct b out of b.h, which defines it.
struct b;

// a.h and b.h include each other. Only the types of b.h use the types of
// a.h, so the types modules form no import cycle and get no boot files.
#include "b.h"

struct a {
  int value;
};

// A function that uses struct b goes to the Safe module, which imports the
// types module of b.h. Functions do not close an import cycle.
void a_takes_b(struct b *b);

#endif
