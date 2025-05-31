#pragma once

struct point {
  int x;
  int y;
};

// This is the library exported function
struct point flip_point(struct point p);

// This is the wrapper we'd generate
//
// `result` must be a pre-allocated buffer
void flip_point_wrapper(struct point *p, struct point* result);

