/* callbacks with structs (by value) */

typedef struct {
  int x;
  int y;
} point;

typedef point point_op (point p);

extern point scale_2_point (point p);

extern point apply_point_op (point_op * f, point p);
