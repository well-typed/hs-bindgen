// Include directives may have macro arguments.  See all of section 6.10
// (Proprocessing directives) in the C99 specification for details.

#define CHILD_HEADER "include_macro_child.h"
#include CHILD_HEADER

struct rect { pt tl, br; };
