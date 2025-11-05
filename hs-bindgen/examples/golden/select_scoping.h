// The header contains a type definition which we will parse, but should not
// select!
#include <select_scoping_header.h>

// This declaration is parsed _and_ selected.
typedef int ParsedAndSelected;
