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
