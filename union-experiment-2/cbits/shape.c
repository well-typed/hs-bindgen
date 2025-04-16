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

void random_shape_details(int* tag, shape_details* details) {
    *tag = rand() % 2;
    switch(*tag) {
        case RECT:
          details->rectangle.x1 = rand() % 100;
          details->rectangle.y1 = rand() % 100;
          details->rectangle.x2 = rand() % 100;
          details->rectangle.y2 = rand() % 100;
          break;
        case CIRCLE:
          details->circle.x = rand() % 100;
          details->circle.y = rand() % 100;
          details->circle.d = (rand() % 100) / 99.0;
          break;
    }
}

double_shape* double_it(shape* s) {
    double_shape* res = malloc(sizeof(double_shape));

    res->tag1 = s->tag;
    res->tag2 = s->tag;

    switch(s->tag) {
        case RECT:
            res->details1.rectangle.x1 = s->details.rectangle.x1;
            res->details1.rectangle.y1 = s->details.rectangle.y1;
            res->details1.rectangle.x2 = s->details.rectangle.x2;
            res->details1.rectangle.y2 = s->details.rectangle.y2;

            res->details2.rectangle.x1 = s->details.rectangle.x1;
            res->details2.rectangle.y1 = s->details.rectangle.y1;
            res->details2.rectangle.x2 = s->details.rectangle.x2;
            res->details2.rectangle.y2 = s->details.rectangle.y2;
            break;
        case CIRCLE:
            res->details1.circle.x = s->details.circle.x;
            res->details1.circle.y = s->details.circle.y;
            res->details1.circle.d = s->details.circle.d;

            res->details2.circle.x = s->details.circle.x;
            res->details2.circle.y = s->details.circle.y;
            res->details2.circle.d = s->details.circle.d;
            break;
    }

    return res;
}