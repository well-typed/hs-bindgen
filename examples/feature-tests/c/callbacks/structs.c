#include "structs.h"

point scale_2_point (point p) {
  p.x *= 2;
  p.y *= 2;
  return p;
};

point apply_point_op (point_op * f, point x) {
  return f(x);
};
