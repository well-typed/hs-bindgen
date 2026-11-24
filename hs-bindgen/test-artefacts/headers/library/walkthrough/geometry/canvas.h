#ifndef GEOMETRY_CANVAS_H
#define GEOMETRY_CANVAS_H

// canvas.h does not include shape.h: it declares struct shape forward. The
// types module of this header still imports the types module of shape.h.

struct shape;

struct canvas {
  int width;
  int height;
  struct shape *selection;
};

void canvas_draw(struct canvas *canvas, const struct shape *shape);

#endif
