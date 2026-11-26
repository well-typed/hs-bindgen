#ifndef VENDOR_MODULE_COLLISION_VENDOR_H
#define VENDOR_MODULE_COLLISION_VENDOR_H

#include <vendor/clock.h>

// With program slicing, struct clock_time goes to the vendor module
// VendorModuleCollision.Vendor, which is also the module name of this header.

struct timer {
  struct clock_time started;
};

#endif
