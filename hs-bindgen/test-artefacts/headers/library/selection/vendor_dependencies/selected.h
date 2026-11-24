#ifndef VENDOR_DEPENDENCIES_SELECTED_H
#define VENDOR_DEPENDENCIES_SELECTED_H

#include "no_vendor.h"

// The variant that selects only this header pulls struct no_vendor in by
// program slicing. It lands in the module of no_vendor.h.
struct selected {
  const struct no_vendor *dependency;
  double offset;
};

double selected_apply(const struct selected *c, int raw);

#endif
