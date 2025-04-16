#pragma once

typedef enum shape_tag {
    RECT = 0,
    CIRCLE
} shape_tag;

typedef struct rectangle {
    int x1;
    int y1;
    int x2;
    int y2;
} rectangle;

typedef struct circle {
    int x;
    int y;
    float d;
} circle;

typedef union shape_details {
    rectangle rectangle;
    circle circle;
} shape_details;

typedef struct shape {
    shape_tag tag;
    shape_details details;
} shape;

typedef struct double_shape {
    shape_tag tag1;
    shape_details details1;
    shape_tag tag2;
    shape_details details2;
} double_shape;

shape* new_rect(int x1, int y1, int x2, int y2);
shape* new_circle(int x, int y, float d);
void print_shape(shape* s);

void print_shape_details(int tag, shape_details* details);
void random_shape_details(int* tag, shape_details* details);

double_shape* double_it(shape* s);