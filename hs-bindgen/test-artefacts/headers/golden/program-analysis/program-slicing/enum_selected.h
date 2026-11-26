#include "enum_selected_t.h"

// T (see enum_selected_t.h) is selected as a transitive dependency of the
// tagged enum E and of the untagged enum constant U_A with program slicing
// enabled (C23, and Clang as an extension, lets an enum fix its underlying
// type)
enum E : T { E_A, E_B };
enum : T { U_A };
