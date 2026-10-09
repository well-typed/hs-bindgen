// vendor_by_tag/lib/alias.h: a typedef for a struct from outside the library.
// It has the name of the struct, so the two are one type in Haskell even
// though user.h uses the struct as well.
#ifndef VENDOR_BY_TAG_ALIAS_H
#define VENDOR_BY_TAG_ALIAS_H

#include <vendor/window.h>

typedef struct vendor_window vendor_window;

#endif
