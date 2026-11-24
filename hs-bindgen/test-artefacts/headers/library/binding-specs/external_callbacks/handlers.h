#ifndef EXTERNAL_CALLBACKS_HANDLERS_H
#define EXTERNAL_CALLBACKS_HANDLERS_H

// The specification that the generated_specs library writes for this header
// covers same_header.
#include "../generated_specs/same_header.h"

// The two callbacks have the same raw function type, which mentions the
// external type same_header. Library mode compares these types when it
// collects the raw function types. That comparison needs the module of
// same_header, which comes from its binding specification.
struct handlers {
  void (*on_open)(struct handlers *self, same_header *arg);
  void (*on_close)(struct handlers *self, same_header *arg);
};

#endif
