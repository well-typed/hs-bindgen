#ifndef LIBRARY_MYLIB_MYLIB_OPS_H
#define LIBRARY_MYLIB_MYLIB_OPS_H

#include "types.h"
double point_distance(point_t a, point_t b);
double rect_area(rect_t r);
int rect_contains(rect_t r, point_t p);

#endif
