// V is defined identically in an included header and in this main header. The
// definition in the main header is kept, so the default selection predicate
// selects V.
#include "main_header_inner.h"
#define V 3
