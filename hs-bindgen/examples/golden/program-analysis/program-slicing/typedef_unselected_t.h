// T is NOT selected, even though it is a transitive dependency of foo (see
// typedef_unselected.h) because program slicing is disabled
typedef int T;