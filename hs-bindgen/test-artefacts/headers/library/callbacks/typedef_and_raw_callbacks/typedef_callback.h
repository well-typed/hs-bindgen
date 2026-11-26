#ifndef TYPEDEF_AND_RAW_CALLBACKS_TYPEDEF_CALLBACK_H
#define TYPEDEF_AND_RAW_CALLBACKS_TYPEDEF_CALLBACK_H

struct event {
  int code;
};

// The ToFunPtr and FromFunPtr instances of a typedef callback go to the types
// module, or to the per-header Callback module in an import cycle.
typedef void (*typedef_callback)(const struct event *ev, void *user);

#endif
