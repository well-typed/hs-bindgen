#ifndef RECT_H
#define RECT_H

typedef struct {
    double x;
    double y;
    double width;
    double height;
} rect;

double rect_area(const rect *r);
double rect_perimeter(const rect *r);

#endif
