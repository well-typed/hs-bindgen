// forward_typedef/lib/foo.h: defines a struct that types.h declares forward,
// and uses lib_status from types.h, so each of the two headers needs something
// from the other
#ifndef FORWARD_TYPEDEF_FOO_H
#define FORWARD_TYPEDEF_FOO_H

#include "types.h"

struct foo {
  bar *owner;
  lib_status status;
};

#endif
