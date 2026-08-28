#ifndef BINDING_SPECS_SPELLING_LEGACY_H
#define BINDING_SPECS_SPELLING_LEGACY_H

/* Reaches core.h from a subdirectory, so clang reports it under a different
   spelling than the <...> form the binding specification is keyed on. The
   specification has to apply anyway: it is the same file.
   See https://github.com/well-typed/hs-bindgen/issues/2236 */
#include "../core.h"

struct spelling_user {
  struct spelling_core core;
};

#endif
