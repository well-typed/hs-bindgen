#ifndef SQUASHED_ACROSS_HEADERS_A_H
#define SQUASHED_ACROSS_HEADERS_A_H

#include "fwd.h"

// struct a and struct b refer to each other through the typedefs of fwd.h.
// The import cycle is between the types modules of a.h and b.h.
struct a {
  b *peer;
  int value;
};

a *a_new(void);

#endif
