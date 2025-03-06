#pragma once

/**
 * Example union
 *
 * This is the equivalent of
 *
 * ```haskell
 * data Dim = Dim2 Int Int | Dim3 Int Int Int
 * ```
 */

typedef struct Dim2 {
    int x;
    int y;
} Dim2_t;

typedef struct Dim3 {
    int x;
    int y;
    int z;
} Dim3_t;

typedef union DimPayload {
    Dim2_t dim2;
    Dim3_t dim3;
} DimPayload_t;

typedef struct Dim {
    int tag;
    DimPayload_t payload;
} Dim_t;

int dim_max(Dim_t* dim);
void dim_grow(Dim_t* in, Dim_t* out);

