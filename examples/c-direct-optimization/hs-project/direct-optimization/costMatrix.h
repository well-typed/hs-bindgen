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

/* A cost matrix library                                                      */

#ifndef COSTMATRIX_H
#define COSTMATRIX_H

#include "debug_constants.h"
#include "dyn_character.h"

/* The encoding type for costs/distances */
#ifndef TYPE_COST
#define TYPE_COST

/**
 *  CHOICE: Should the cost encoding be "wide?"
 *    0. 32-bits (No)
 *    1. 64-bits (Yes)
 *
 *  NOTE:
 *    -  Always use 'cost_t' as the type for cost values
 *    -  Always use 'cost_p' within the formatting string for 'printf' calls.
 */
#define WIDE_COST 1

#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

#if WIDE_COST == 1
// #define cost_t uint64_t
typedef uint64_t cost_t;
#define cost_p PRIu64
#else
// #define cost_t uint32_t
typedef uint32_t cost_t;
#define cost_p PRIu32
#endif
#endif /* TYPE_COST */


#define Cost_matrix_struct(a) ((struct cost_matrices_2d *) Data_custom_val(a))
#define Cost_matrix_struct_3d(a) ((struct cost_matrices_3d *) Data_custom_val(a))

/**
 * Check cost_matrices_3d for further information. This is the corresponding data
 * structure for two dimensional character alignment.
 */
typedef struct cost_matrices_2d_t {
    size_t alphSize;            /* alphabet size including gap, and including ambiguities if
                                 * combinations == True
                                 */
    size_t costMatrixDimension; /* n in an n x n matrix;
                                 * n is the powerset of the alphabet size (including gap), minus the empty set.
                                 * or 2 ^ alphSize - 1.
                                 */
    elem_t gap_char;            // gap character value (1 << (alphSize - 1))
    int cost_model_type;        /* The type of cost model to be used in the alignment,
                                 * i.e. affine or not.
                                 * Based on cost_matrix.ml, values are:
                                 * • linear == 0
                                 * • no_alignment == 1 ** I don't believe this is every used.
                                 * • affine == 2
                                 */
    int include_ambiguities;    /* This is a flag set to true if we are going to accept
                                 * all possible combinations of the elements in the alphabet
                                 * in the alignments. This is not true for protein characters
                                 * for example, where the number of elements of the alphabet
                                 * is already too big to build all the possible combinations.
                                 */
    cost_t gap_open_cost; /* The cost of opening a gap. For affine (type 2). */
    int is_metric;              /* if tcm is symmetric
                                 *
                                 * -- MISSING IN 3D
                                 */
    size_t num_elements;        /** total number of elements. This is alphSize if we're using only unambiguous elems,
                                 *  otherwise |power set|
                                 */
    cost_t *cost;         /* The transformation cost matrix, including ambiguities,
                                 * storing the **best** cost for each ambiguity pair
                                 */
    elem_t *median;             /* The matrix of possible medians between elements in the
                                 * alphabet. The best possible medians according to the cost
                                 * matrix.
                                 */
    cost_t *worst;        /* The transformation cost matrix, including ambiguities,
                                 * storing the **worst** cost for each ambiguity pair
                                 *
                                 * -- MISSING IN 3D
                                 */
    cost_t *prepend_cost; /* The cost of going from gap -> each base. For ambiguities, use best cost.
                                 * Set up as all_elements x all_elements
                                 * matrix, but seemingly only first row is used.               <-- TODO: fix this, and in tail_cost below.
                                 * Missing in 3d because current version of 3d sets gap cost
                                 * as constant.
                                 *
                                 * -- MISSING IN 3D
                                 */
    cost_t *tail_cost;    /* As prepend_cost, but with reverse directionality,
                                 * so base -> gap.
                                 * As with prepend_cost, seems to be allocated as too large.
                                 * Missing in 3d because current version of 3d sets gap cost
                                 * as constant.
                                 *
                                 * -- MISSING IN 3D
                                 */
} cost_matrices_2d_t;


/** A three dimesional cost matrix
 *
 * For three way character alignment, this structure holds the cost of
 * transforming the elements of an alphabet.  A cost matrix can only be applied
 * on a particular alphabet.
 */
typedef struct cost_matrices_3d_t {
    size_t alphSize;             /** The number of symbols in the alphabet */
    size_t costMatrixDimension;  /** n in an n x n x n matrix;
                                  *  n is the powerset of the alphabet size (including gap), minus the empty set.
                                  *  or 2 ^ alphSize - 1.
                                  */
    elem_t gap_char;             /** The integer representing a gap character in the alphabet */
    int cost_model_type;         /** The type of cost model to be used in the alignment */
    int include_ambiguities;     /** This is a flag set to true if we are going to accept
                                  *  all possible combinations of the elements in the alphabet
                                  *  in the alignments. This is not true for protein characters
                                  *  for example, where the number of elements of the alphabet
                                  *  is already too big to build all the possible combinations.
                                  */
    cost_t gap_open_cost;        /** The cost of opening a gap. This is only useful in
                                  *  certain cost_model_types.
                                  */
    size_t num_elements;         /** The integer that represents all the combinations, used
                                  *  for ambiguities
                                  */
    cost_t *cost;                /** The transformation cost matrix. The ordering is row-major,
                                  *  and the lookup is a->b, where a is a row label and b is
                                  *  a column label
                                  */
    elem_t *median;              /** The matrix of possible medians between elements in the
                                  *  alphabet. The best possible medians according to the cost
                                  *  matrix.
                                  */
} cost_matrices_3d_t;


void cm_print_2d ( cost_matrices_2d_t *c );


/** Print one of matrices in costMatrix_2d struct
 *  don't take entire cost matrix because you want to print only medians or costs.
 *  Need a height and a width because prepend, worst and tail_cost are only height 1.
 */
void cm_print_matrix_2d
  ( cost_t *costMatrix
  , size_t  height
  , size_t  alphSize
  );


/**
 * Creates a cost matrix with memory allocated for an alphabet of size alphSize
 * (not including the gap representation which is internally chosen), and whose
 * size must consider all possible combinations of characters in the alphabet
 * iff combinations != 0. Set the affine gap model parameters to the values
 * stored in do_aff, gap_open, in the cost matrix res.
 * In case of error the function fails with the message "Memory error.".
 */
void cm_alloc_2d
  ( cost_matrices_2d_t *res
  , size_t              alphSize
  , size_t              combinations
  , int                 do_aff
  , cost_t              gap_open_cost
  , int                 is_metric
  , size_t              num_elements
  );


void
cm_set_cost_2d
  ( cost_matrices_2d_t *costMtx
  , elem_t              elem1
  , elem_t              elem2
  , cost_t              v
  );


cost_t *
cm_get_row
  ( cost_t *tcm
  , elem_t  a
  , size_t  alphSize
  );


/**
 * Gets the total number of possible combinations of an alphabeet of size
 * alphSize. The size of the alphabet must be bigger than 0.
 */
//static inline int
//cm_combinations_of_alphabet (const int alphSize);

/**
 * Calculates the median position in a transformation cost matrix for an
 * alphabet of size alphSize and elements a and b.
 */
/*
static inline int
cm_calc_median_position (elem_t a, elem_t b, int alphSize);
*/


/**
 * The median between to elements in the alphabet hold by t.
 * @param t is a transformation cost matrix
 * @param a is an element in the alphabet of t
 * @param b is an element in the alphabet of t
 * @return an element in the alphabet of t which is a median between a and b
 * according to the transformation cost matrix hold in t.
 */
elem_t
cm_get_median_2d
  ( const cost_matrices_2d_t *t
  ,       elem_t              a
  ,       elem_t              b
  );


/**
 * Retrieves the transformation cost of the elements a and b as stored in the
 * transformation cost matrix tcm, containing information for an alphabet of
 * size alphSize.
 */
cost_t
cm_calc_cost_2d
  ( cost_t *tcm
  , elem_t  a
  , elem_t  b
  , size_t  alphSize
  );


/**
 * Gets the row in the transformation cost matrix tcm where the transformations
 * of character a are located, when tcm holds information for an alphabet of
 * size alphSize.
 */
/*
static inline int *
cm_get_row ( int *tcm
           , elem_t a
           , int alphSize
           );
*/

/* set the value of c->worst at location (a,b) to v */
void
cm_set_worst
  ( cost_matrices_2d_t *c
  , size_t              a
  , size_t              b
  , cost_t              v
  );


/**
 * Gets the precalculated row for a particular character in the alphabet.
 * @param p is the precalculated matrix.
 * @param item is the element in the alphabet that produced p that should be
 *  generated.
 * @param len is the length of the character that was source of the precalculated
 *  matrix.
 */
cost_t *
cm_get_precal_row
  ( cost_t *p
  , elem_t  item
  , size_t  len
  );


/**
 * Deallocates the memory structure iff there are no more pointers to it,
 * otherwise it will just decrease the garbage collection counter.
 */
void
cm_free ( cost_matrices_2d_t *costMtx );


int
cm_get_gap_opening_parameter ( cost_matrices_2d_t *costMtx );


cost_t
cm_get_cost_2d
  ( cost_matrices_2d_t *costMtx
  , elem_t              a
  , elem_t              b
  );


cost_t
cm_get_value_2d
  ( elem_t  a
  , elem_t  b
  , cost_t *p
  , size_t  alphSize
  );


void
cm_set_prepend_2d
  ( cost_matrices_2d_t *costMtx
  , int                 a
  , int                 b
  );


void
cm_set_tail_2d
  ( cost_matrices_2d_t *costMtx
  , int                 a
  , int                 b
  );


void
cm_set_median_2d
  ( cost_matrices_2d_t *costMtx
  , elem_t              a
  , elem_t              b
  , cost_t              v
  );


void
cm_set_value_2d
  ( elem_t  a
  , elem_t  b
  , cost_t  v
  , cost_t *p
  , int alphSize
  );


void cm_print_3d ( cost_matrices_3d_t *c );


/** Print one of matrices in costMatrix_3d struct
 *  don't take entire cost matrix because you want to print only medians or costs.
 *  Only need matrix dimension because both median and cost matrices are cubic.
 */
void
cm_print_matrix_3d
  ( cost_t *costMatrix
  , size_t  costMatrixDimension
  );


void
cm_set_cost_3d
  ( cost_matrices_3d_t *costMtx
  , elem_t              elem1
  , elem_t              elem2
  , elem_t              elem3
  , cost_t              v
  );


/** As with 2d, but doesn't compute worst, prepend or tail */
void
cm_alloc_3d
  ( cost_matrices_3d_t *res
  , int                 alphSize
  , int                 combinations
  , int                 do_aff
  , int                 gap_open
  , int                 all_elements
  );


/**
 * The median between three alphabet elements a, b and c.
 * @param t is the transformation cost matrix
 * @param a is the first element in the alphabet
 * @param b is the second element in the alphabet
 * @param c is the third element in the alphabet
 * @return an element of the alphabet contained in t that provides the best
 * median between a, b, and c, according to the transformation cost matrix
 * contained in t.
 */
elem_t
cm_get_median_3d
  ( const cost_matrices_3d_t *matrix
  ,       elem_t              a
  ,       elem_t              b
  ,       elem_t              c
  );


/**
 * Gets the row in the transformation cost matrix tcm where the transformations
 * of character a are located, when tcm holds information for an alphabet of
 * size alphSize.
 */
cost_t *
cm_get_row_3d
  ( cost_t *tcm
  , elem_t  char1
  , elem_t  char2
  , size_t  alphSize
  );


cost_t
cm_get_cost_3d
  ( const cost_matrices_3d_t *costMtx
  ,       elem_t              elem1
  ,       elem_t              elem2
  ,       elem_t              elem3
  );


void
cm_set_median_3d
  ( const cost_matrices_3d_t *costMtx
   ,      elem_t              elem1
   ,      elem_t              elem2
   ,      elem_t              elem3
   ,      elem_t              val
   );


void
cm_print_median
  ( elem_t *m
  , size_t  w
  , size_t  h
  );


#define max(a,b) (a > b) ? (a) : (b)

#define min(a,b) (a < b) ? (a) : (b)

void
printVolumeOfMedianValues
  ( const cost_matrices_3d_t *matrix
  , size_t                    dimension
  , elem_t                    i
  , elem_t                    j
  , elem_t                    k
  );

#endif /* COSTMATRIX_H */
