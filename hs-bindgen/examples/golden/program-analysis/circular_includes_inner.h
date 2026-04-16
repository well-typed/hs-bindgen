typedef int INNER_BEFORE_IFNDEF;
#ifndef INNER_H
#define INNER_H
typedef int INNER_BEFORE_CIRCULAR_INCLUDE;
#include "circular_includes.h"
typedef int INNER_AFTER_CIRCULAR_INCLUDE;
#endif
typedef int INNER_AFTER_IFNDEF;
