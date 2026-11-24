#ifndef GEOMETRY_POINT_H
#define GEOMETRY_POINT_H

// The first header of the library that the library mode documentation builds
// up step by step. It includes nothing.

struct point {
  double x;
  double y;
};

extern const struct point point_origin;

double point_distance(const struct point *a, const struct point *b);

#endif
