#ifndef SQUASHED_ACROSS_HEADERS_HANDLE_H
#define SQUASHED_ACROSS_HEADERS_HANDLE_H

#include "fwd.h"

// The typedef is squashed into struct handle of private/handle.h. This header
// keeps only functions: it writes Safe, Unsafe and FunPtr modules but no
// types module, and no module imports a types module for it.
typedef struct handle handle;

handle *handle_new(a *owner);
int handle_use(handle *h);

#endif
