#ifndef INNER_H
typedef int OUTER_BEFORE_CIRCULAR_INCLUDE;
#include "circular_includes_inner.h"
typedef int OUTER_AFTER_CIRCULAR_INCLUDE;
#endif
