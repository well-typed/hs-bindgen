#ifndef INCLUDE_CYCLE_B_H
#define INCLUDE_CYCLE_B_H

#include "a.h"

struct b {
  struct a *a;
};

#endif
