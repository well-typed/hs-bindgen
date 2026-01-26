#include "pt.h"

// When struct dual is translated to an opaque type, neither struct pt nor the
// anonymous structs should be be translated.
union dual {
  struct { struct pt tl, br; } rect;
  struct { struct pt tl, br; } diag;
};
