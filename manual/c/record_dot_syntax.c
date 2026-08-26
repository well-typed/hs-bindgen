#include "record_dot_syntax.h"

#include <stdint.h>

/* -------------------------------------------------------------------------- */
/* Record dot syntax */

/* -------------------------------------------------------------------------- */
/* Example: tagged unions */

const Shape c_example_Rectangle = {
      .tag = rectangle_tag
    , .rectangle.top_left.x  = 3
    , .rectangle.top_left.y  = 7
    , .rectangle.bot_right.x = 9
    , .rectangle.bot_right.y = -17
    };

Shape c_move_x (int delta, Shape s) {
  switch (s.tag) {
    case rectangle_tag:
      s.rectangle.top_left.x += delta;
      s.rectangle.bot_right.x += delta;
      break;
    case circle_tag:
      s.circle.mid.x += delta;
      break;
    default:
      break;
  };
  return s;
};
