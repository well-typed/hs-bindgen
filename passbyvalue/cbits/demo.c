#include <stdlib.h>

#include "demo.h"

// Library provided
struct point flip_point(struct point p) {
    struct point result;
    result.x = p.y;
    result.y = p.x;
    return result;
}

// Generated
void flip_point_wrapper(struct point *p, struct point *result) {
    *result = flip_point(*p);
}
