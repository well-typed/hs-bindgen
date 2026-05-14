// Macro and typedef scope example (M1 M3 T2 T4) in two files.

// This test checks that the declarations of multiple includes in one file are
// sorted in the correct order.

// Declarations of "inner1.h" need to be sorted before declarations of
// "inner2.h". If this is not the case, reparse of `T2` fails, and we create
// wrong (albeit functional) bindings. The detailed reasons are:
//
// Header "inner1.h" defines
//
// #define M1 int
// typedef M1 T2;
//
// Header "inner2.h" defines
//
// #define M3 T2
// typedef M3 T4;
//
// 1. We parse the declaration and the order of parse results is: M1 M3 T2 T4.
//
// 2. When we construct the use-decl graph, we sort the declarations according
// to our "order map" obtained from the include graph. In an older revision, the
// order map sorted sibling includes in reverse order (i.e., "inner2.h" before
// "inner1.h"): M3 T4 M1 T2.
//
// 3. We detect a dependency of M3 on T2, moving T2 before M3: T2 M3 T4 M1.
//
// 4. We fail to reparse T2, because it depends on M1, a fact that we _can not
// detect_ before reparse.
//
// After fixing `topSort'`, the order after 2. is M1 T2 M3 T4, and the test
// passes.

#include "macro_typedef_scope_multiple_inner1.h"

#include "macro_typedef_scope_multiple_inner2.h"
