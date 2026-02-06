// T is NOT selected, even though it is a transitive dependency of foo (see
// macro_unselected.h) because program slicing is disabled
#define T int
