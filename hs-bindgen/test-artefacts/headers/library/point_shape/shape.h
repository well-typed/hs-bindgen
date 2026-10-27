#ifndef LIBRARY_POINT_SHAPE_SHAPE_H
#define LIBRARY_POINT_SHAPE_SHAPE_H

#include "point.h"
struct shape { struct point centre; int sides; };
typedef status (*on_draw)(struct shape *s);

#endif
