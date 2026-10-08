// The third header of a small library, after stopwatch.h and counter.h.
//
// Library mode generates one module per header, all with the same
// prescriptive binding specification (report_p.yaml), which omits
// `struct stopwatch`. The modules of the first two headers both record that
// omission in their binding specifications (stopwatch.yaml and counter.yaml,
// as library mode wrote them). The module of this header reads both.

#include "counter.h"

struct report {
  struct counter reads;
  struct counter writes;
};
