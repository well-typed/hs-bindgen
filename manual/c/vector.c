#include <stdlib.h>

#include "vector.h"

vector* new_vector(double x, double y) {
    vector* res = malloc(sizeof(vector));
    res->x = x;
    res->y = y;
    return res;
}