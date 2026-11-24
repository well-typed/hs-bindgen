#include "enum_unselected_t.h"

// T (see enum_unselected_t.h) is NOT selected as a transitive dependency of
// the tagged enum E and of the untagged enum constant U_A because program
// slicing is disabled (C23, and Clang as an extension, lets an enum fix its
// underlying type)
//
// A warning message should be printed for E and U_A, which are not selected
// due to the missing transitive dependency on T
enum E : T { E_A, E_B };
enum : T { U_A };

// We would expect that we generate bindings only for F
enum F { F_A };
