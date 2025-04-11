#include <stdlib.h>

#include "shape.h"

shape* new_rect(int x1, int y1, int x2, int y2) {
    shape* res = malloc(sizeof(shape));
    res->tag = RECT;
    res->details.rectangle.x1 = x1;
    res->details.rectangle.y1 = y1;
    res->details.rectangle.x2 = x2;
    res->details.rectangle.y2 = y2;
    return res;
}

shape* new_circle(int x, int y, float d) {
    shape* res = malloc(sizeof(shape));
    res->tag = CIRCLE;
    res->details.circle.x = x;
    res->details.circle.y = y;
    res->details.circle.d = d;
    return res;
}
