// mylib/types.h -- shared type definitions
#ifndef MYLIB_TYPES_H
#define MYLIB_TYPES_H

typedef struct {
  double x;
  double y;
} point_t;

typedef struct {
  point_t origin;
  double width;
  double height;
} rect_t;

#endif
