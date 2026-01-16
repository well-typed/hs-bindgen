// The header contains a type definition which we will parse, but should not
// select!
#include "select_scoping_header.h"

// This declaration is parsed _and_ selected.
typedef int ParsedAndSelected1;

// This declaration is parsed _and_ selected. The dependency is parsed but not
// selected (by default).
typedef ParsedAndNotSelected ParsedAndSelected2;

// This declaration is parsed _and_ selected. The dependency is _not_ parsed nor
// selected (custom parse predicate).
typedef struct ParseNotAttemptedNotSelected ParsedAndSelected3;

// This declaration is parsed and selected. The dependency is _not_ parsed nor
// selected.
struct ParsedUnselectable {
  struct ParseNotAttemptedNotSelected x;
};

// This declaration is parsed _and_ selected. The dependency is parsed but
// unselectable because another transitivie dependency is unusable.
// `ParsedAndSelected4` is squashed, and so we do not get direct trace messages
// in the select pass (but a squash notice).
typedef struct ParsedUnselectable ParsedAndSelected4;

// This declaration is parsed _and_ selected. However, a transitive dependency
// is unusable.
typedef ParsedAndSelected4 ParsedAndSelected5;
