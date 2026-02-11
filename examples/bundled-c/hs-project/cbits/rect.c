#include "rect.h"

double rect_area(const rect *r) {
    return r->width * r->height;
}

double rect_perimeter(const rect *r) {
    return 2.0 * (r->width + r->height);
}
