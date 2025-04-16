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

shape* new_rect(int x1, int y1, int x2, int y2);
shape* new_circle(int x, int y, float d);

