#ifndef VENDOR_DEPENDENCIES_USES_VENDOR_H
#define VENDOR_DEPENDENCIES_USES_VENDOR_H

#include <vendor/vendor.h>
#include "no_vendor.h"

// Every declaration here depends on a vendor declaration. With the default
// selection and no program slicing, none is selected and this header is empty.

struct uses_vendor {
  const struct no_vendor *source;
  struct vendor_struct taken_at;
  double value;
};

struct uses_vendor uses_vendor_take(const struct no_vendor *s);
vendor_typedef uses_vendor_age(const struct uses_vendor *s);

#endif
