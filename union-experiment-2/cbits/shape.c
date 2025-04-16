#include <stdio.h>
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

void print_shape_details(int tag, shape_details* details) {
    switch(tag) {
        case RECT:
            printf("{%d, %d, %d, %d}\n",
                details->rectangle.x1,
                details->rectangle.y1,
                details->rectangle.x2,
                details->rectangle.y2);
            break;
         case CIRCLE:
            printf("{%d, %d, %f}\n",
                details->circle.x,
                details->circle.y,
                details->circle.d);
            break;
    }
}

void print_shape(shape* s) {
    print_shape_details(s->tag, &s->details);
}