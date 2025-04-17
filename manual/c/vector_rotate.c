#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#include "vector_rotate.h"

vector* vector_rotate(vector* v, double th) {
    vector* res = malloc(sizeof(vector));
    res->x = cos(th) * v->x - sin(th) * v->y;
    res->y = sin(th) * v->x + cos(th) * v->y;
    return res;
}