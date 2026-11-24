#ifndef GEOMETRY_SCENE_H
#define GEOMETRY_SCENE_H

#include "layer.h"

// The macro uses a macro of layer.h, through the boot file of its module.
#define SCENE_MAX_SHAPES (LAYER_MAX_SHAPES * 8)

struct scene {
  struct layer *layers;
  int count;
};

struct scene *scene_new(void);

#endif
