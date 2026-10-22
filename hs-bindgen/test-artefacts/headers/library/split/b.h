#ifndef SPLIT_B_H
#define SPLIT_B_H

#include "a.h"

struct foo { foo next; };

// Bar and bar_x again: they clash with 'struct bar' wherever they meet it.
typedef int bar;
#define bar_x 1

#endif
