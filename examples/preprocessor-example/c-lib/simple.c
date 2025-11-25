#include "simple.h"

Point add_points(Point a, Point b) {
    Point result;
    result.x = a.x + b.x;
    result.y = a.y + b.y;
    return result;
}

int distance_squared(Point p) {
    return p.x * p.x + p.y * p.y;
}
