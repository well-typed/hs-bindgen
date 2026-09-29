// f is declared in an included header and again in this main header. The
// declaration in the main header is kept, so the default selection predicate
// selects f.
#include "redeclaration_main_header_inner.h"
int f(int x);
