#ifndef INCLUDE_GRAPH_WIDGET_GADGET_H
#define INCLUDE_GRAPH_WIDGET_GADGET_H

/* Reaches core.h by a path containing '..', so clang reports it under a
   different SourcePath than the '<widget/core.h>' spelling used elsewhere. */
#include "../widget/core.h"

struct include_graph_gadget {
  struct include_graph_core core;
};

#endif
