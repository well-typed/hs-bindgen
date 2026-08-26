#pragma once

/* -------------------------------------------------------------------------- */
/* Record dot syntax */

/* -------------------------------------------------------------------------- */
/* Example: tagged unions */

typedef struct { int x; int y; } Point;

typedef struct {
  Point top_left;
  Point bot_right;
} Rectangle;

typedef struct {
  Point mid;
  unsigned radius;
} Circle;

enum Shape_Tag { rectangle_tag, circle_tag };

typedef struct {
  enum Shape_Tag tag;
  union {
    Rectangle rectangle;
    Circle circle;
  };
} Shape;

extern const Shape c_example_Rectangle;

Shape c_move_x (int delta, Shape s);
