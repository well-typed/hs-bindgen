#ifndef INCLUDE_GRAPH_OTHER_REVERSED_H
#define INCLUDE_GRAPH_OTHER_REVERSED_H

/* Same two headers as other.h, in the other order. clang keeps the name of a
   file's first lookup, so this reaches core.h through gadget.h's '..' spelling
   first, and the plain spelling second. */
#include <widget/gadget.h>
#include <widget/core.h>

struct include_graph_other_reversed {
  struct include_graph_core core;
};

#endif
