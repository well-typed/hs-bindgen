#ifndef INCLUDE_GRAPH_WIDGET_COMPAT_LEGACY_H
#define INCLUDE_GRAPH_WIDGET_COMPAT_LEGACY_H

/* Reaches core.h from a subdirectory, so clang reports it as a path with a
   '..' segment running through 'compat'. A header path regex matched against
   that raw path sees 'compat' even though core.h is not in compat/. */
#include "../core.h"

struct include_graph_legacy {
  struct include_graph_core core;
};

#endif
