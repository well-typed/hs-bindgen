/* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    */
/* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  */
/* and the American Museum of Natural History.                                */
/*                                                                            */
/* This program is free software; you can redistribute it and/or modify       */
/* it under the terms of the GNU General Public License as published by       */
/* the Free Software Foundation; either version 2 of the License, or          */
/* (at your option) any later version.                                        */
/*                                                                            */
/* This program is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/* GNU General Public License for more details.                               */
/*                                                                            */
/* You should have received a copy of the GNU General Public License          */
/* along with this program; if not, write to the Free Software                */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   */
/* USA                                                                        */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "costMatrix.h"

/**
 * Function signature declarations
 */
void
cm_print_matrix_3d_cost
  ( cost_t *costMatrix
  , size_t costMatrixDimension
  );


void
cm_print_matrix_3d_elem
  ( elem_t *costMatrix
  , size_t costMatrixDimension
  );
/*
void cm_set_value_2d ( elem_t a, elem_t b, elem_t v, elem_t *p, int alphSize );
cost_t cm_get_value_2d (elem_t a, elem_t b, cost_t *p, size_t alphSize );
void cm_set_cost_2d (cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2, cost_t val );

cost_t
cm_get_cost_2d ( cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2 );


void cm_set_prepend_2d ( cost_matrices_2d_t *c, int a, int b );

void
cm_set_tail_2d ( cost_matrices_2d_t *c, int a, int b );

int cm_compare ( cost_matrices_2d_t *a, cost_matrices_2d_t *b );
*/


static inline int
ceil_log_2 (int v)
{
    int i = 0;
    while (v != 0) {
        i++;
        v = v >> 1;
    }
    return (i + 1);
}

static inline int
cm_combinations_of_alphabet (const int alphSize)
{
    assert (alphSize > 0);
    return (1 << alphSize);
}


/** Each address in the array is divided into two parts. Shift first element's value to the left alphSize,
 *  then || with second element's value. That will give you a size_t that holds the address into the array
 *  for the two elements.
 */
static inline size_t
cm_calc_cost_position_2d (elem_t a, elem_t b, size_t alphSize)
{
    assert (alphSize > 0);
    return (size_t) (a << alphSize) + b;
}


void cm_print_2d (cost_matrices_2d_t *costMatrix)
{
    printf("\nCost matrix fields:\n");
    printf("  alphabet size:        %zu\n", costMatrix->alphSize);
    printf("  costMatrixDimension:  %zu\n", costMatrix->costMatrixDimension);
    printf("  gap_char:             %d\n",  costMatrix->gap_char);
    printf("  cost model:           %d\n",  costMatrix->cost_model_type);
    printf("  include ambiguities:  %d\n",  costMatrix->include_ambiguities);
    printf("  gap open:             %" cost_p "\n",  costMatrix->gap_open_cost);
    printf("  is metric:            %d\n",  costMatrix->is_metric);
    printf("  num elements:         %zu\n", costMatrix->num_elements);

    printf("\n  Cost matrix:\n    ");
    cm_print_matrix_2d(costMatrix->cost,         costMatrix->costMatrixDimension, costMatrix->costMatrixDimension);
    printf("  Prepend cost\n    ");
    cm_print_matrix_2d(costMatrix->prepend_cost, 1,                        costMatrix->costMatrixDimension);
//    printf("  Worst\n    ");
//    cm_print_matrix(c->worst,        costMatrix->costMatrixDimension, costMatrix->costMatrixDimension);
    printf("  Tail cost:\n    ");
    cm_print_matrix_2d(costMatrix->tail_cost,    1,                         costMatrix->costMatrixDimension );
//    printf("  Median costs:\n    ");
//    cm_print_matrix_2d(costMatrix->median,        costMatrix->costMatrixDimension, costMatrix->costMatrixDimension);
}


void
cm_print_matrix_2d
  ( cost_t *costMatrix
  , size_t  height
  , size_t  dimension
  )
{
    for (size_t i = 0; i < height; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (size_t j = 0; j < dimension; j++)
            printf ("%4" cost_p, *(costMatrix + (dimension * i) + j));
        printf ("\n    ");
    }
}


void
cm_free ( cost_matrices_2d_t *costMatrix )
{
    if (NULL != costMatrix)
    {
        if (NULL != costMatrix->cost)         free(costMatrix->cost);
        if (NULL != costMatrix->median)       free(costMatrix->median);
        if (NULL != costMatrix->worst)        free(costMatrix->worst);
        if (NULL != costMatrix->prepend_cost) free(costMatrix->prepend_cost);
        if (NULL != costMatrix->tail_cost)    free(costMatrix->tail_cost);
        costMatrix->cost = costMatrix->prepend_cost
                         = costMatrix->tail_cost
                         = costMatrix->worst
                         = NULL;
        costMatrix->median = NULL;
        free(costMatrix);
    }
}

/** As with cm_calc_cost_position_2d, but with three elements, so first element gets shifted alphSize twice, etc. */
static inline size_t
cm_calc_position_3d (elem_t a, elem_t b, elem_t c, size_t alphSize)
{
    assert(alphSize > 0);
    assert(a > 0 && "Cannot have an empty element!");
    assert(b > 0 && "Cannot have an empty element!");
    assert(c > 0 && "Cannot have an empty element!");
    return (((a << alphSize) + b) << alphSize) + c;
}


static inline void
cm_set_affine (cost_matrices_2d_t *costMatrix, int do_aff, int gapOpenCost)
{
    assert(costMatrix != NULL);
    costMatrix->cost_model_type = do_aff;
    costMatrix->gap_open_cost   = gapOpenCost;
}


/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model parameters to the values
 * stored in do_aff, gap_open_cost, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void
cm_alloc_2d ( cost_matrices_2d_t *res
            , size_t              alphSize
            , size_t              combinations
            , int                 do_aff
            , cost_t        gap_open_cost
            , int                 is_metric
            , size_t              num_elements
            )
{
    if (DEBUG_COST_M)  printf("\n---cm_alloc_set_costs_2d\n");

    res->gap_char = 1;
    if (combinations) {
        res->alphSize            = alphSize;
        res->costMatrixDimension = cm_combinations_of_alphabet (alphSize); // 2 ^ alphSize - 1 is |power set of alphSize|
        res->include_ambiguities = 1;
    } else {
        res->alphSize            = alphSize;
        res->costMatrixDimension = ceil_log_2 (alphSize + 1);
        res->include_ambiguities = 0;
    }

    res->num_elements = num_elements;
    res->is_metric    = is_metric;

    cm_set_affine (res, do_aff, gap_open_cost);

    size_t size = 2
                * (res->costMatrixDimension)
                * (res->costMatrixDimension)
                * sizeof(int); // size for cost matrices

    assert( size != 0 && "Your cost matrix is too large to fit in memory. I can't continue with your data loading." );
        // Here and below, fail if the dimension of the cost matrix is so large it overflows 64 bits.

    res->cost         = calloc (size, 1);
    res->worst        = calloc (size, 1);
    res->prepend_cost = calloc (size, 1);
    res->tail_cost    = calloc (size, 1);
    res->median       = calloc (size, 1);

    assert(   res->cost         != NULL
           && res->worst        != NULL
           && res->prepend_cost != NULL
           && res->median       != NULL
           && res->tail_cost    != NULL
           && "OOM: Cannot allocate 2D cost matrices." );

    assert(   res->cost         != NULL
           && res->worst        != NULL
           && res->prepend_cost != NULL
           && res->tail_cost    != NULL
           && "OOM: Couldn't alloc 2d cost matrix fields." );

    size = 2
         * (res->costMatrixDimension)
         * (res->costMatrixDimension)
         * sizeof(elem_t); // size for median matrix

    assert( 0 != size && "Your cost matrix is too large to fit in memory. I can't continue with your data loading." );
}


static inline elem_t
cm_calc_median_2d
  ( elem_t *tcm
  , elem_t a
  , elem_t b
  , int alphSize
  )
{
    elem_t *res;
    cost_t one        = 1;
    cost_t upperBound = one << alphSize;
    assert (alphSize   >= 0);
    assert (upperBound >  a);
    assert (upperBound >  b);
    res = tcm + cm_calc_cost_position_2d(a, b, alphSize);
    return (*res);
}

cost_t
cm_calc_cost_2d ( cost_t *tcm
                , elem_t        a
                , elem_t        b
                , size_t        alphSize
                )
{
    cost_t one = 1;
    cost_t upperBound = one << alphSize;
    cost_t boundBound = 8 * sizeof(cost_t); // Otherwise we rotate the bits over
    assert (alphSize   >  0);
    assert (alphSize   < boundBound);
    assert (upperBound > a);
    assert (upperBound > b);
    return tcm[ cm_calc_cost_position_2d(a, b, alphSize) ];
}


cost_t *
cm_get_row (cost_t *tcm, elem_t a, size_t alphSize) {
    cost_t one = 1;
    cost_t upperBound = one << alphSize;

    assert( alphSize   > 0 && "Alphabet size >= 3." );
    if (upperBound <= a) {
        printf("\nupperBound:(%" cost_p ") <= a:(%u)\n", upperBound, a);
        fflush(stdout);
        assert( upperBound > a && "The value of element 'a' is larger than the maximum allowed value based on the alphabet size." );
    }
    return tcm + (a << alphSize);
}


/*
void
cm_set_value_2d (elem_t a, elem_t b, cost_t v, cost_t *p, int alphSize) {
    p[ cm_calc_cost_position_2d (a, b, alphSize) ] = v;
}


cost_t
cm_get_value_2d (elem_t a, elem_t b, cost_t *p, size_t alphSize) {
    return p[ cm_calc_cost_position_2d (a, b, alphSize) ];
}
*/


void
cm_set_cost_2d (cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2, cost_t val) {
    costMtx->cost[ cm_calc_cost_position_2d (elem1, elem2, costMtx->alphSize) ] = val;
}

cost_t
cm_get_cost_2d (cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2) {
    return costMtx->cost[ cm_calc_cost_position_2d (elem1, elem2, costMtx->alphSize) ];
}

void
cm_set_prepend_2d (cost_matrices_2d_t *c, int a, int b) {
    c->prepend_cost[a] = b;
}

void
cm_set_tail_2d (cost_matrices_2d_t *c, int a, int b) {
    c->tail_cost[a] = b;
}

void
cm_set_median_2d (cost_matrices_2d_t *costMtx, elem_t elem1, elem_t elem2, cost_t val) {
    costMtx->median[ cm_calc_cost_position_2d (elem1, elem2, costMtx->alphSize) ] = val;
}


int
cm_compare (cost_matrices_2d_t *a, cost_matrices_2d_t *b) {
    int cmp, mtxElementCount;
    size_t lenCosts, lenElems;
    if      (a->alphSize != b->alphSize)                       return (a->alphSize            - b->alphSize);
    else if (a->costMatrixDimension != b->costMatrixDimension) return (a->costMatrixDimension - b->costMatrixDimension);
    else if (a->include_ambiguities != b->include_ambiguities) return (a->include_ambiguities - b->include_ambiguities);
    else if (a->cost_model_type != b->cost_model_type)         return (a->cost_model_type     - b->cost_model_type);
    else if (a->gap_open_cost != b->gap_open_cost)             return (a->gap_open_cost       - b->gap_open_cost);
    else if (a->is_metric != b->is_metric)                     return (a->is_metric           - b->is_metric);
    else {
        mtxElementCount = 2 * (a->costMatrixDimension) * (a->costMatrixDimension);
        lenCosts = mtxElementCount * sizeof(int);
        lenElems = mtxElementCount * sizeof(elem_t);
        cmp = memcmp ( a->cost, b->cost, lenCosts );

        if (cmp != 0)    return cmp;

        cmp = memcmp ( a->median, b->median, lenElems );
        //if (cmp != 0) return (cmp);
        //cmp   = memcmp (a->worst, b->worst, len);
        return cmp;
    }
}

void
cm_copy_contents (int *src, int *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
}


void
cm_copy_contents_dyn_char_p (elem_t *src, elem_t *tgt, int len) {
    int i;
    for (i = 0; i < len; i++)
        *(tgt + i) = *(src + i);
}


elem_t
cm_get_median_2d ( const cost_matrices_2d_t *t
                 ,       elem_t             a
                 ,       elem_t             b
                 ) {
    return cm_calc_median_2d( t->median, a, b, t->alphSize );
}


void cm_print_3d (cost_matrices_3d_t *costMatrix)
{
    printf("\nCost matrix fields:\n");
    printf("  alphabet size:        %zu\n",  costMatrix->alphSize);
    printf("  costMatrixDimension:  %zu\n",  costMatrix->costMatrixDimension);
    printf("  gap_char:             %u\n",   costMatrix->gap_char);
    printf("  cost model:           %d\n",   costMatrix->cost_model_type);
    printf("  include ambiguities:  %d\n",   costMatrix->include_ambiguities);
    printf("  gap open:             %" cost_p "\n", costMatrix->gap_open_cost);
    printf("  num elements:         %zu\n",  costMatrix->num_elements);

    printf("\n  Cost matrix:\n    ");
    cm_print_matrix_3d_cost(costMatrix->cost,   costMatrix->costMatrixDimension + 1);
    printf("  Median costs:\n    ");
    cm_print_matrix_3d_elem(costMatrix->median, costMatrix->costMatrixDimension + 1);
}


// don't take entire cost matrix because I want to print only
void
cm_print_matrix_3d_cost
  ( cost_t *costMatrix
  , size_t costMatrixDimension
  )
{

    for (size_t i = 1; i < costMatrixDimension; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (size_t j = 1; j < costMatrixDimension; j++) {
            for (size_t k = 1; k < costMatrixDimension; k++) {
                printf ("%4" cost_p, costMatrix[cm_calc_position_3d(i, j, k, costMatrixDimension)]);
            }
            printf ("\n");
            for (size_t num_tabs = 1; num_tabs < j && j < costMatrixDimension - 1; num_tabs++) {
                printf("    ");
            }
        }
    }
    printf ("\n    ");
}


// don't take entire cost matrix because I want to print only
void
cm_print_matrix_3d_elem
  ( elem_t *costMatrix
  , size_t costMatrixDimension
  )
{

    for (size_t i = 1; i < costMatrixDimension; i++) {
        //fprintf(stdout,"%zu: ", i);
        for (size_t j = 1; j < costMatrixDimension; j++) {
            for (size_t k = 1; k < costMatrixDimension; k++) {
                printf ("%4" elem_p, costMatrix[cm_calc_position_3d(i, j, k, costMatrixDimension)]);
            }
            printf ("\n");
            for (size_t num_tabs = 1; num_tabs < j && j < costMatrixDimension - 1; num_tabs++) {
                printf("    ");
            }
        }
    }
    printf ("\n    ");
}


void
cm_3d_free (cost_matrices_2d_t *costMatrix)
{
    if(NULL != costMatrix->cost)   free (costMatrix->cost);
    if(NULL != costMatrix->median) free (costMatrix->median);
    if(NULL != costMatrix)         free (costMatrix);
}


static inline void
cm_set_affine_3d (cost_matrices_3d_t *costMatrix, int do_aff, int gapOpenCost)
{
    assert(costMatrix != NULL);
    costMatrix->cost_model_type = do_aff;
    costMatrix->gap_open_cost   = gapOpenCost;
}


/*
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabeet
 * iff combinations != 0. Set the affine gap model paramters to the values
 * stored in do_aff, gap_open_cost, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void
cm_alloc_3d ( cost_matrices_3d_t *res
            , int                alphSize
            , int                combinations
            , int                do_aff
            , int                gap_open_cost
            , int                num_elements
            )
{
    int size;
    if (DEBUG_COST_M) {
        printf ("Allocating a three dimensional matrix:\n");
        printf ("alphabet size: %d \n", alphSize);
        printf ("combinations:  %d \n", combinations);
        printf ("cost model:    %d \n", do_aff);
        printf ("gap open cost: %d \n", gap_open_cost);
    }
    // TODO: check the following code. Does combinations need to be reset? Are the dimensions set right?
    res->gap_char = 1;
    if (combinations != 0) {
        res->alphSize            = alphSize;
        res->costMatrixDimension = cm_combinations_of_alphabet (alphSize); // 2 ^ alphSize - 1 is |power set of alphSize|
        res->include_ambiguities = 1;
    } else {
        res->alphSize            = alphSize;
        res->costMatrixDimension = ceil_log_2 (alphSize + 1);
        res->include_ambiguities = 0;
    }

    res->num_elements = num_elements;
    cm_set_affine_3d (res, do_aff, gap_open_cost);
    size              = (res->costMatrixDimension + 1)
                      * (res->costMatrixDimension + 1)
                      * (res->costMatrixDimension + 1);

    res->cost         = calloc (size * sizeof(cost_t), 1);
    res->median       = calloc (size * sizeof(elem_t), 1);
    assert(   res->cost   != NULL
           && res->median != NULL
           && "Memory error during cost matrix allocation." );
}


inline int
cm_get_gap_opening_parameter_3d (const cost_matrices_3d_t *c) {
    assert( c != NULL && "No cost matrix provided.");
    return c->gap_open_cost;
}


// static inline size_t
// cm_calc_position_3d (int a, int b, int c, size_t alphSize) {
//     assert(alphSize >= 0);
//     return (((a << alphSize) + b) << alphSize) + c;
// }


cost_t
cm_get_value_3d_cost
  ( const cost_t *p
  ,       elem_t a
  ,       elem_t b
  ,       elem_t c
  ,       size_t alphSize
  )
{
    return p[ cm_calc_position_3d (a, b, c, alphSize) ];
}


elem_t
cm_get_value_3d_elem
  ( const elem_t *p
  ,       elem_t a
  ,       elem_t b
  ,       elem_t c
  ,       size_t alphSize
  )
{
    return p[ cm_calc_position_3d (a, b, c, alphSize) ];
}


elem_t
cm_get_median_3d( const cost_matrices_3d_t *matrix
                ,       elem_t              a
                ,       elem_t              b
                ,       elem_t              c
                )
{
    cost_t upperBound = ((elem_t) 1) << matrix->alphSize;
    if (DEBUG_3D) printf( "alphSize: %zu, upperBound: %2" cost_p ";  elements a: %2u, b: %2u, c: %2u;  median: %2u\n"
                        , matrix->alphSize
                        , upperBound
                        , a
                        , b
                        , c
                        , matrix->median[ cm_calc_position_3d (a, b, c, matrix->alphSize) ]
                        );

    assert( matrix->alphSize > 0 && "Alphabet size < 0." );
    assert( upperBound       > a && "Element a has a larger than allowed value." );
    assert( upperBound       > b && "Element b has a larger than allowed value." );
    assert( upperBound       > c && "Element c has a larger than allowed value." );

    return cm_get_value_3d_elem(matrix->median, a, b, c, matrix->alphSize);
}


cost_t *
cm_get_row_3d
  ( cost_t *tcm
  , elem_t  char1
  , elem_t  char2
  , size_t  alphSize
  )
{
    cost_t one = 1;
    cost_t upperBound = one << alphSize;

    assert( alphSize   > 0     && "Alphabet size should be > 0.");
    assert( upperBound > char1 && "Character 1 is bigger than alphabet size." );
    assert( upperBound > char2 && "Character 2 is bigger than alphabet size." );
    return (tcm + (((char1 << alphSize) + char2) << alphSize));
}


/*
static inline void
cm_set_value_3d_cost
    ( cost_t *matrix_array
    , elem_t        elem1
    , elem_t        elem2
    , elem_t        elem3
    , cost_t        val
    , size_t        alphSize
    )
{
    matrix_array[cm_calc_position_3d (elem1, elem2, elem3, alphSize)] = val;
}


static inline void
cm_set_value_3d_elem
    ( elem_t *matrix_array
    , elem_t        elem1
    , elem_t        elem2
    , elem_t        elem3
    , elem_t        val
    , size_t        alphSize
    )
{
    matrix_array[cm_calc_position_3d (elem1, elem2, elem3, alphSize)] = val;
}
*/


void
cm_set_cost_3d
  ( cost_matrices_3d_t *costMtx
  , elem_t              elem1
  , elem_t              elem2
  , elem_t              elem3
  , cost_t              val
  )
{
    costMtx->cost[cm_calc_position_3d (elem1, elem2, elem3, costMtx->alphSize)] = val;
}

cost_t
cm_get_cost_3d
  ( const cost_matrices_3d_t *costMtx
  ,       elem_t              elem1
  ,       elem_t              elem2
  ,       elem_t              elem3
  )
{
    return cm_get_value_3d_cost (costMtx->cost, elem1, elem2, elem3, costMtx->alphSize);
}


void
cm_set_median_3d
  ( const cost_matrices_3d_t *costMtx
  ,       elem_t              elem1
  ,       elem_t              elem2
  ,       elem_t              elem3
  ,       elem_t              val
  )
{
    costMtx->median[cm_calc_position_3d (elem1, elem2, elem3, costMtx->alphSize)] = val;
}


void printVolumeOfMedianValues(const cost_matrices_3d_t *matrix, size_t dimension, elem_t i, elem_t j, elem_t k)
{
    int    left_offset  = dimension / 2;
    elem_t right_offset = (elem_t) left_offset + (dimension % 2 == 1 ? 1 : 0);
    elem_t bound = matrix->costMatrixDimension - 1;

    // We set the lower bound here to 1 and not 0 because there cannot be empty elements!
    elem_t aStart = max(1, (int) i - left_offset);
    elem_t bStart = max(1, (int) j - left_offset);
    elem_t cStart = max(1, (int) k - left_offset);

    elem_t aBound = min (bound, i + right_offset);
    elem_t bBound = min (bound, j + right_offset);
    elem_t cBound = min (bound, k + right_offset);

    assert(aBound < matrix->costMatrixDimension);
    assert(bBound < matrix->costMatrixDimension);
    assert(cBound < matrix->costMatrixDimension);

    printf("Point: (%d, %d, %d)\n", i, j, k);
    printf("Range: [%d, %d] X [%d, %d] X [%d, %d] \n\n", aStart, aBound - 1, bStart, bBound - 1, cStart, cBound - 1);

    for (elem_t a = aStart; a < aBound; ++a) {
        elem_t total_tabs = 1;
        for (elem_t b = bStart; b < bBound; ++b, ++total_tabs) {
            for (elem_t c = cStart; c < cBound; ++c) {
                printf ("%4" elem_p, matrix->median[ cm_calc_position_3d(a, b, c, matrix->alphSize)]);
            }
            printf ("\n");
            for (size_t num_tabs = 0; num_tabs < total_tabs && b < bBound - 1; num_tabs++) {
                printf("    ");
            }
        }
    }
    printf ("\n");
}
