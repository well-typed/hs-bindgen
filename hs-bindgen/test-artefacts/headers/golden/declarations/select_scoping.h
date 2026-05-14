// The header contains a type definition which we will parse, but should not
// select!
#include "select_scoping_header.h"

// This declaration is parsed _and_ selected.
typedef int ParsedAndSelected1;

// This declaration is parsed _and_ selected. The dependency is parsed but not
// selected.
typedef ParsedAndNotSelected ParsedAndSelected2;

// This declaration is parsed _and_ selected. However, a recursive transitive
// dependency is unusable.
typedef ParsedAndSelected2 ParsedAndSelected3;
