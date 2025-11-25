#ifndef SIMPLE_H
#define SIMPLE_H

typedef struct {
    int x;
    int y;
} Point;

Point add_points(Point a, Point b);
int distance_squared(Point p);

#endif // SIMPLE_H
