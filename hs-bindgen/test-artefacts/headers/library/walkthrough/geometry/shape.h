#ifndef GEOMETRY_SHAPE_H
#define GEOMETRY_SHAPE_H

#include "point.h"

#define SHAPE_MAX_POINTS 64

struct shape {
  struct point *points;
  int count;
};

// A typedef callback: its instances stay in the types module of this header.
typedef int (*shape_filter)(const struct shape *shape, void *user);

struct shape *shape_new(int count);
int shape_count_matching(struct shape **shapes, int count, shape_filter filter, void *user);

// A callback type written in place that mentions a library type: its
// instances go to the global Callback module.
void shape_for_each_point(struct shape *shape, void (*visit)(struct point *point, void *user), void *user);

#endif
