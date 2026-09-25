// A header without include guard, included twice. Both definitions of G come
// from the same line; they are identical, so G is a benign redefinition.
#include "noguard_inner.h"
#include "noguard_inner.h"
