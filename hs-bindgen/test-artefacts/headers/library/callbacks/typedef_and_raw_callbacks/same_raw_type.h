#ifndef TYPEDEF_AND_RAW_CALLBACKS_SAME_RAW_TYPE_H
#define TYPEDEF_AND_RAW_CALLBACKS_SAME_RAW_TYPE_H

#include "raw_callbacks.h"

// The same raw function pointer type as in raw_callbacks.h. The global
// Callback module has one instance for both headers.
void also_takes_raw_callback(struct loop *l, void (*callback)(struct loop *l, int n));

#endif
