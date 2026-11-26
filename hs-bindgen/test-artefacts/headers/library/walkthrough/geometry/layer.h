#ifndef GEOMETRY_LAYER_H
#define GEOMETRY_LAYER_H

#include "shape.h"

// struct layer and struct scene point at each other, so the types modules of
// layer.h and scene.h import each other with SOURCE imports. layer.h does not
// include scene.h.

#define LAYER_MAX_SHAPES 32

struct scene;

struct layer {
  struct scene *scene;
  struct shape *shapes;
  int count;
};

// A typedef callback of a header in an import cycle: its instances go to the
// Callback module of this header.
typedef void (*layer_paint)(struct layer *layer, void *user);

void layer_paint_all(struct layer *layer, layer_paint paint, void *user);

#endif
