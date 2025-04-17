#include <math.h>

#include "vector_length.h"

len vector_length(vector* p) {
    return sqrt(p->x * p->x + p->y * p->y);
}