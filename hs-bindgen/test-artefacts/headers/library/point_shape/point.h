#ifndef LIBRARY_POINT_SHAPE_POINT_H
#define LIBRARY_POINT_SHAPE_POINT_H

typedef int status;
struct shape;
struct point { int x; int y; struct shape *owner; };

#endif
