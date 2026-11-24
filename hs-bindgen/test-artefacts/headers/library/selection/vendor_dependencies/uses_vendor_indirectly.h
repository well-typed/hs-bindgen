#ifndef VENDOR_DEPENDENCIES_USES_VENDOR_INDIRECTLY_H
#define VENDOR_DEPENDENCIES_USES_VENDOR_INDIRECTLY_H

#include "uses_vendor.h"

// Depends on a vendor declaration through struct uses_vendor.
struct uses_vendor_indirectly {
  struct uses_vendor last;
  double alpha;
};

double uses_vendor_indirectly_apply(struct uses_vendor_indirectly *f, double value);

#endif
