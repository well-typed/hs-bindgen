#include "example.h"

int maxint(int a, int b) {
    return a > b ? a : b;
}

int dim_max(Dim_t* dim) {
    switch(dim->tag) {
        case 0: {
            Dim2_t* payload = &(dim->payload.dim2);
            return maxint(payload->x, payload->y);
        }
        default: {
            Dim3_t* payload = &(dim->payload.dim3);
            return maxint(payload->x, maxint(payload->y, payload->z));
        }
    };
}

void dim_grow(Dim_t* in, Dim_t* out) {
    int max = dim_max(in);
    Dim_t result = { tag: 1, payload: { dim3: { x: max, y: max, z: max } } };
    *out = result;
}
