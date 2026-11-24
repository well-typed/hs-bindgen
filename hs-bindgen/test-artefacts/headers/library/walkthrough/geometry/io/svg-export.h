#ifndef GEOMETRY_IO_SVG_EXPORT_H
#define GEOMETRY_IO_SVG_EXPORT_H

#include "../shape.h"

// A header in a subdirectory, with a dash in its name and only a function:
// it gets the Safe, Unsafe and FunPtr modules of Geometry.Io.Svg_export, and
// no types module.

int svg_export(const struct shape *shape, const char *path);

#endif
