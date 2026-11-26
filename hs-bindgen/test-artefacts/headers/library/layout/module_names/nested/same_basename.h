#ifndef MODULE_NAMES_NESTED_SAME_BASENAME_H
#define MODULE_NAMES_NESTED_SAME_BASENAME_H

#include "../same_basename.h"
#include "../dash-name.h"

// Same file name as ../same_basename.h, in a subdirectory.
struct same_basename {
  int width;
  struct dash_name colour;
};

#endif
