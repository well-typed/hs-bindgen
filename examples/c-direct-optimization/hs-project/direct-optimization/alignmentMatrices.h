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

#ifndef NWMATRICES_H
#define NWMATRICES_H

#include "costMatrix.h"
#include "dyn_character.h"

/** The following consts are to define possible moves in an NW matrix.
 *  As we're only saving one possible matrix, we don't need ambiguities,
 *  Thus for 2d we only have 3 possible states, rather than 7.
 *
 *  Remember that we bias toward the shorter character, so INSERT puts a gap
 *  in the longer character and DELETE puts a gap in the shorter character. TODO: make sure shorter character is on left
 *
 *  Likewise, for 3d we should need only 7 states and not 2^7 - 1.
 */
#define DIAGONAL (1 << 0)
#define BEHIND   (1 << 1)
#define UPPER    (1 << 2)
#define ALIGN    DIAGONAL
#define INSERT   BEHIND
#define DELETE   UPPER
#define SHIFT_V  3
#define SHIFT_H  6
#define ALIGN_V  (ALIGN << SHIFT_V)
#define DELETE_V (DELETE << SHIFT_V)
#define ALIGN_H  (ALIGN << SHIFT_H)
#define INSERT_H (INSERT << SHIFT_H)
#define G_A_G    (1 << 0)     /** Previously P1. Move in pages (i.e., put gaps in for 1 & 3). */
#define A_A_G    (1 << 1)     /** Previously P2. Move in column and page. */
#define A_G_G    (1 << 2)     /** Previously P3. Move in columns */
#define G_A_A    (1 << 3)     /** Previously S1. Move in page and row. */
#define A_A_A    (1 << 4)     /** Previously S2. Move in all three. */
#define A_G_A    (1 << 5)     /** Previously S3. Move in column and row. */
#define G_G_A    (1 << 6)     /** Previously SS. Move in rows. */

// TODO: Can this be a char, instead?
#define DIR_MTX_ARROW_t  unsigned short

#define Matrices_struct(a) ((struct nwMatrices_t *) Data_custom_val(a))

typedef struct alignment_matrices_t {
            /****** In each of the following calculations, character length includes opening gap *******/
    size_t           cap_nw;           /** Total length of available memory allocated to matrix or cube ==
                                        *    12 * max(len_s1, len_s2)
                                        */
    size_t           cap_eff;          /** Length of the efficiency matrix; at least as large as cap_nw.
                                        *  int because is originally set as -1
                                        */ // TODO: figure out what this actually is
    size_t           cap_pre;          /** Length of the precalculated matrix == max(len_s1, len_s2) * (alphSize + 1)
                                        *  ---extra 1 is for gap
                                        */
    cost_t          *algn_costMtx;     /** NW cost matrix for 2d alignment */
    DIR_MTX_ARROW_t *algn_dirMtx;      /** Matrix for backtrace directions in a 2d alignment */
    cost_t          *algn_precalcMtx;  /** a three-dimensional matrix that holds
                                         *  the transition costs for the entire alphabet (of all three characters)
                                         *  with the character char3. The columns are the bases of char3, and the rows are
                                         *  each of the alphabet characters (possibly including ambiguities). See
                                         *  cm_precalc_4algn_3d for more information).
                                         */
} alignment_matrices_t;


// void algnMtx_print(alignment_matrices_t *m, size_t alphSize);


/*
 * Fills a precalculated matrix with the cost of comparing each elment in the
 * character inChar with each element in the alphabet specified in the transformation
 * cost matrix costMtx.
 *
 * @param costMtx is the transformation cost matrix to calculate the precalculated
 *  vectors.
 * @param toOutput is the matrix that will hold the output.
 * @param s is the character for which the cost matrix will be precalculated.
 *
 * This function is only valid for two dimensional alignments.
 * TODO: why is this in cm instead of matrices?
 */
void
algnMtx_precalc_4algn_2d
  (       alignment_matrices_t *alignmentMatrices
  , const cost_matrices_2d_t   *costMatrix
  , const dyn_character_t      *inChar
  );


cost_t *
algnMtx_get_precal_row
  ( cost_t *p
  , elem_t  item
  , size_t  len
  );


/*
 * Calculates the amount of memory required to perform a two dimensional
 * alignment between characters of length w and d. This is a small amount of
 * memory, so no ukkonen barriers for this.
 */
size_t
algnMat_size_of_2d_matrix
  ( size_t w
  , size_t h
  );


/*
 * Rearrange or reallocate memory if necessary to perform an alignment between
 * characters of length w, d and h. Note that for 2d alignments is necessary to
 * set h=0, and uk=0.
 * Order of characters is unimportant here, as just reallocing.
 */
void
algnMat_setup_size
  ( alignment_matrices_t *m
  , size_t                len_char1
  , size_t                len_char2
  , size_t                matrixDimension
  );


/* Printout the contents of the matrix */
void
algnMat_print_algn_2d
  ( alignment_matrices_t *m
  , size_t                w
  , size_t                h
  );


#endif /* NWMATRICES_H */
