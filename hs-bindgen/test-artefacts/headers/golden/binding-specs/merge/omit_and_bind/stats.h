// The third header of a small library, after stopwatch.h and operation.h.
//
// Library mode generated the module of stopwatch.h with a prescriptive
// binding specification for that module alone (stopwatch_p.yaml, which has
// an `hsmodule`). It omits `struct stopwatch`, and the binding specification
// of that module records the omission (stopwatch.yaml). The specification
// does not apply to the module of operation.h, which needs the struct, so
// that module generates it and records the binding (operation.yaml). The
// module of this header reads both.

#include "operation.h"

struct stats {
  struct stopwatch started;
  struct operation reads;
  struct operation writes;
};
