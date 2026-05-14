#include "pt.h"

// When struct rect is translated to an opaque type, struct pt should not be
// translated.
struct rect { struct pt tl, br; };

// When struct foo is translated to an opaque type, the anonymous struct should
// not be translated.
struct foo { struct { int a, b; } bar, baz; };

// When struct oaa is translated to an opaque type, neither anonymous struct
// should be translated.
struct oaa {
  struct {
    struct {
      int e, f;
    } c, d;
  } a, b;
};

// When struct oan is translated to an opaque type, the anonymous struct should
// not be translated but struct named should be translated because it may still
// be used even when struct oan is opaque.
struct oan {
  struct {
    struct named {
      int e, f;
    } c, d;
  } a, b;
};
