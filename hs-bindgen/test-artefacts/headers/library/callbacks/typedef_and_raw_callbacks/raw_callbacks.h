#ifndef TYPEDEF_AND_RAW_CALLBACKS_RAW_CALLBACKS_H
#define TYPEDEF_AND_RAW_CALLBACKS_RAW_CALLBACKS_H

#include "typedef_callback.h"

// Raw function pointer types that mention library types get their instances
// in the global Callback module.
struct loop {
  typedef_callback typedef_field;
  void (*raw_field)(struct loop *l);
};

void takes_typedef_callback(struct loop *l, typedef_callback callback, void *user);
void takes_raw_callback(struct loop *l, void (*callback)(struct loop *l, int n));

// A raw function pointer type with primitive types only gets no instance.
void takes_primitive_raw_callback(void (*callback)(const char *message));

#endif
