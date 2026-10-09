// rpm_pair/td.h: includes types.h, and defines the struct behind rpmtd with a
// field of a type from types.h.  The two headers use each other.
#ifndef RPM_PAIR_TD_H
#define RPM_PAIR_TD_H

#include "types.h"

struct rpmtd_s {
  rpm_tag tag;
  int count;
};

int rpmtd_count(rpmtd td);

#endif
