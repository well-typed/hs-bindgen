#ifndef GEOMETRY_PRIVATE_SHAPE_H
#define GEOMETRY_PRIVATE_SHAPE_H

// A private header next to shape.h. The leading underscore is dropped from
// module names, so both headers get the module name Geometry.Shape. Only the
// root header of the module_name_collision step includes this header.

struct shape_cache {
  int dirty;
};

#endif
