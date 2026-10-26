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

/******************************************************************************/
/*                        Pairwise Standard Alignment                         */
/******************************************************************************/
/*
 * As standard, all the caml binding functions are called algn_CAML_<function
 * name>
 */

                                                  /**************** IMPORTANT!!! ****************/

/**************** Note that all included 3d functions are beta and unused. Instead UkkCommon code, due to Powell, is used. ****************/

/** Fill a row in a two dimensional alignment
 *
 *  When pairwise alignment is performed, two dynamic characters are compared over a
 *  transformation cost matrix. Let's call them characters x and y, written over
 *  some alphabet a of length |a|. Each base of x
 *  is represented by a column in the transformation cost matrix and each base of
 *  y by a row. However, note that the actual values that are added during the
 *  alignment are produced by comparing every single base of x with only |a|
 *  elements. Now, in order to make these operations vectorizable, we perform
 *  the comparisons with |a| precalculated vectors. This puts in context the
 *  explanation of each parameter in the function.
 *
 *  @param curRow is the cost matrix row to be filled with values.
 *  @param prevRow is the row above curRow in the cost matrix being filled.
 *  @param gap_row is the cost of aligning each base in x with a gap.
 *  @param align_row is the cost of aligning each base in x with the base
 *      represented by the base of the row of curRow in y.
 *  @param dirMtx is the directional matrix for the backtrace
 *  @param c is the cost of an insertion. An insertion can only occur for one
 *      particular base in the alphabet, corresponding to the base in y represented
 *      by the row that is being filled.
 *  @param st is the starting cell to be filled.
 *  @param end is the final cell to be filled.
 *
 *  If you modify this code check algn_fill_3dMtx as there is similar code there
 *  used in the first plane of the alignment. I didn't use this function because
 *  the direction codes are different for three dimensional alignments.
 */

#include <assert.h>
#include <limits.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>


#include "alignCharacters.h"
#include "costMatrix.h"
#include "debug_constants.h"
#include "ukkCommon.h"

#define ALIGN_TO_ALIGN      1
#define ALIGN_TO_VERTICAL   2
#define ALIGN_TO_HORIZONTAL 4
#define ALIGN_TO_DIAGONAL   8
#define BEGIN_BLOCK         16
#define END_BLOCK           32
#define BEGIN_VERTICAL      64
#define END_VERTICAL        128
#define BEGIN_HORIZONTAL    256
#define END_HORIZONTAL      512
#define DO_ALIGN            1024
#define DO_VERTICAL         2048
#define DO_HORIZONTAL       4096
#define DO_DIAGONAL         8192
// DO_DIAGONAL MUST BE THE LAST ONE

#define LOR_WITH_DIR_MTX_ARROW_t(mask, direction_matrix) direction_matrix |= mask


#ifdef DEBUG_ALL_ASSERTIONS
int *_algn_max_matrix = NULL;
DIR_MTX_ARROW_t *_algn_max_direction = NULL;
#endif


#if ( __GNUC__ && __MMX__ )
static inline void
algn_fill_row (       cost_t    *curRow
              , const cost_t    *prevRow
              , const cost_t    *gap_row
              , const cost_t    *align_row
              ,       DIR_MTX_ARROW_t *dirMtx
              ,       int              c
              ,       int              i
              ,       int              end
              )
{

    register int aa, bb, cc;
    register const int TWO  = 0x200;
    register const int ZERO = 0;

    bb = curRow[i - 1];

    for (; i <= end - 7; i+=8) {

        aa  = prevRow[i - 1] + align_row[i]; // aka tmp3
        bb += gap_row[i];                    // aka tmp2
        cc  = prevRow[i] + c;                // aka tmp1

        /**
         *  The algorithm has not changed. Only the conditional branches been eliminated for
         *  better performance.
         *
         *  Since gcc (4.0.3 at least) didn't generate cmov's, we changed the code manually.
         *  Things that have been done to optimize this function:
         *      - assembly code for determining min(aa, bb, cc) and getting the bit pattern
         *        loop unrolling with a factor of 8, to still keep this function inlined
         *      - rearrangement of expressions => better register usage and (probably)
         *        fewer cache misses

         *  Restrictions:
         *  ALIGN is bound to the value 4, INSERT to 2 and DELETE to 1
         *  If these constants change, there will be problems with the assembler code,
         *  since it is really optimized for this purpose

         *  Furthermore, this code only works with the gnu compiler (but shouldn't be
         *  hard to switch to icc), so add
         *  #ifdef __GNUC__ as a macro for this section. I also suppose that cmov's
         *  can be handled by most of the computers used today
         *  (of those using this program at least).

         *  I have also removed the debug sections. These can of course be added if
         *  a debug is needed.
         *  I recommend that debugging is only used with the original function,
         *  since this one generates exactly same results.
         */


        __asm__(
            "cmp %0, %1\n\t"    // compare aa with bb (the needed cmp flag remains even if registers are switched)
            "cmovg %0, %1\n\t"  // if bb > aa was flagged, put aa into bb's register. Now we know that bb is always the smallest value
            "mov $0x0, %0\n\t"  // aa is now trash and will not be used anymore. Clean the register (set it to 0x0)
            "cmovle %4, %0\n\t" // if bb <= aa was flagged in the first instruction, then put TWO (0x200) into register 0 (aa)
            "setge %b0\n\t"     // if bb >= aa was flagged in the first instruction, then set the lowest byte in register 0 to 1 (0x01)
            "cmp %1, %2\n\t"    // compare reg.1 (the lowest of aa and bb) with cc.
            "cmovl %2, %1\n\t"  // if cc < bb was flagged in the 2nd comparison, then move cc into bb. min(aa, bb, cc) is now stored in bb.
            "mov $0x4, %2\n\t"  // put a 4 (ALIGN) in register 2 (cc) since cc is trash.
            "cmovl %3, %0\n\t"  // if cc < bb was flagged in the 2nd comparison, then clear register 0.
            "cmovg %3, %2\n\t"  // if cc > bb was flagged, then clean the register. (Note that you can only move register->register with a cmov.)
            "add %b0, %h0\n\t"  // finally, add low byte and high byte of register 0 into register 0's low byte.
            "add %h0, %b2\n\t"  // add high byte from reg.0 to low byte in reg.2 (cc) and we are done. cc contains now the bitpattern.
            : "+Q" (aa), "+r" (bb), "+Q" (cc)   // registers: aa = %0, bb = %1, cc = %2
            : "r" (ZERO), "r" (TWO)         // ZERO = %3, TWO = %4
        );

        curRow[i] = bb; // bb is min(aa, bb, cc)
        dirMtx[i] = cc; // cc is the bitpattern


        aa = prevRow[i] + align_row[i + 1];
        bb += gap_row[i + 1]; // bb is already assigned the minimum value of the three to be compared, so loading bb from memory would be waste.
        cc = prevRow[i + 1] + c;


        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t"
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t"
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );

        curRow[i + 1] = bb;
        dirMtx[i + 1] = cc;

        aa = prevRow[i + 1] + align_row[i + 2];
        bb += gap_row[i + 2];
        cc = prevRow[i + 2] + c;


        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t"
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t"
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );

        curRow[i + 2] = bb;
        dirMtx[i + 2] = cc;

        aa = prevRow[i + 2] + align_row[i + 3];
        bb += gap_row[i + 3];
        cc = prevRow[i + 3] + c;


        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t"
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t"
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );

        curRow[i + 3] = bb;
        dirMtx[i + 3] = cc;



        aa = prevRow[i + 3] + align_row[i + 4];
        bb += gap_row[i + 4];
        cc = prevRow[i + 4] + c;


        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t"
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t"
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );

        curRow[i + 4] = bb;
        dirMtx[i + 4] = cc;


        aa = prevRow[i + 4] + align_row[i + 5];
        bb += gap_row[i + 5];
        cc = prevRow[i + 5] + c;


        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t"
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t"
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );

        curRow[i + 5] = bb;
        dirMtx[i + 5] = cc;

        aa = prevRow[i + 5] + align_row[i + 6];
        bb += gap_row[i + 6];
        cc = prevRow[i + 6] + c;


        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t"
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t"
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );

        curRow[i + 6] = bb;
        dirMtx[i + 6] = cc;

        aa = prevRow[i + 6] + align_row[i + 7];
        bb += gap_row[i + 7];
        cc = prevRow[i + 7] + c;


        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t"
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t"
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );

        curRow[i + 7] = bb;
        dirMtx[i + 7] = cc;
    }



    for (; i <= end; i++) {

        aa = prevRow[i - 1] + align_row[i];
        bb += gap_row[i];
        cc = prevRow[i] + c;


        __asm__(
            "cmp %0, %1\n\t"
            "cmovg %0, %1\n\t"
            "mov $0x0, %0\n\t"
            "cmovle %4, %0\n\t"
            "setge %b0\n\t"
            "cmp %1, %2\n\t"
            "cmovl %2, %1\n\t"
            "mov $0x4, %2\n\t"
            "cmovl %3, %0\n\t"
            "cmovg %3, %2\n\t"
            "add %b0, %h0\n\t"
            "add %h0, %b2\n\t"
            : "+Q" (aa), "+r" (bb), "+Q" (cc)
            : "r" (ZERO), "r" (TWO)
        );

        curRow[i] = bb;
        dirMtx[i] = cc;
    }
}
#else // __GNUC__


static inline void
algn_fill_row (       cost_t    *currRow
              , const cost_t    *prevRow
              , const cost_t    *gap_row
              , const cost_t    *align_row
              ,       DIR_MTX_ARROW_t *dirVect
              ,       int               c
              ,       size_t            startIndex
              ,       size_t            finalIndex
              )
{
    cost_t upwardCost,
                 leftwardCost,
                 diagonalCost;

    for (size_t i = startIndex; i <= finalIndex; i++) {
        // try align with substitution
        upwardCost   = prevRow[i    ] + c;
        leftwardCost = currRow[i - 1] + gap_row[i];
        diagonalCost = prevRow[i - 1] + align_row[i];
        /* check whether insertion is better */
        /* This option will allow all the possible optimal paths to be stored
         * concurrently on the same backtrace matrix. This is important for the
         * characters being able to choose the appropriate direction while
         * keeping the algorithm that assumes that char2 is at most as long as char1.
         * */
        if (upwardCost < diagonalCost) {
            if (upwardCost < leftwardCost) {
                currRow[i] = upwardCost;
                dirVect[i] = DELETE;
            } else if (leftwardCost < upwardCost) {
                currRow[i] = leftwardCost;
                dirVect[i] = INSERT;
            } else {
                currRow[i] = leftwardCost;
                dirVect[i] = INSERT | DELETE;
            }
        } else if (diagonalCost < upwardCost) {
            if (diagonalCost < leftwardCost) {
                currRow[i] = diagonalCost;
                dirVect[i] = ALIGN;
            } else if (leftwardCost < diagonalCost) {
                currRow[i] = leftwardCost;
                dirVect[i] = INSERT;
            } else {
                currRow[i] = leftwardCost;
                dirVect[i] = ALIGN | INSERT;
            }
        } else { // diagonalCost == upwardCost
            if (diagonalCost < leftwardCost) {
                currRow[i] = diagonalCost;
                dirVect[i] = ALIGN | DELETE;
            } else if (leftwardCost < diagonalCost) {
                currRow[i] = leftwardCost;
                dirVect[i] = INSERT;
            } else {
                currRow[i] = leftwardCost;
                dirVect[i] = DELETE | INSERT | ALIGN;
            }
        }
        if (DEBUG_DIR_M) {
        // Print the alignment matrix
            if (INSERT & dirVect[i])
                printf ("I");
            if (DELETE & dirVect[i])
                printf ("D");
            if (ALIGN & dirVect[i])
                printf ("A");
            printf ("\t");
        }
    }
    if (DEBUG_DIR_M)  {
        printf ("\n");
        fflush (stdout);
    }

}
#endif // __GNUC__


static inline void
algn_fill_ukk_right_cell (       cost_t *curRow
                         , const cost_t *prevRow
                         , const cost_t *gap_row
                         , const cost_t *align_row
                         , DIR_MTX_ARROW_t    *dirMtx
                         // ,       int c
                         ,       size_t         pos
                         )
{
    int leftwardCost, diagonalCost;
    /* try align with substitution */
    leftwardCost = curRow[pos - 1]  + gap_row[pos];
    diagonalCost = prevRow[pos - 1] + align_row[pos];

    /* check whether insertion is better */
    if (leftwardCost < diagonalCost) {
        curRow[pos] = leftwardCost;
        dirMtx[pos] = INSERT;
    } else if (diagonalCost < leftwardCost) {
        curRow[pos] = diagonalCost;
        dirMtx[pos] = ALIGN;
    } else { // diagonal cost == leftward cost
        curRow[pos] = diagonalCost;
        dirMtx[pos] = INSERT | ALIGN;
    }
    if (DEBUG_DIR_M) {
        /* Print the alignment matrix */
        if (INSERT & dirMtx[pos])
            printf ("I");
        if (DELETE & dirMtx[pos])
            printf ("D");
        if (ALIGN & dirMtx[pos])
            printf ("A");
        printf ("\t");
    }
    if (DEBUG_COST_M) {
        /* Print the cost matrix */
        printf ("%" cost_p "\t", curRow[pos]);
        fflush (stdout);
    }
    if (DEBUG_COST_M || DEBUG_DIR_M) {
        printf ("\n");
        fflush (stdout);
    }
}

static inline void
algn_fill_ukk_left_cell (       cost_t    *curRow
                        , const cost_t    *prevRow
                       // , const int *gap_row
                        , const cost_t    *align_row
                        ,       DIR_MTX_ARROW_t *dirMtx
                        ,       size_t           c
                        ,       size_t           pos
                        )
{
    int upwardCost, diagonalCost;
    /* try align with substitution */
    upwardCost   = prevRow[pos] + c;
    diagonalCost = prevRow[pos - 1] + align_row[pos];
        if (upwardCost < diagonalCost) {
            curRow[pos] = upwardCost;
            dirMtx[pos] = DELETE;
        } else if (diagonalCost < upwardCost) {
            curRow[pos] = diagonalCost;
            dirMtx[pos] = ALIGN;
        } else { // diagonal cost == upward cost
            curRow[pos] = upwardCost;
            dirMtx[pos] = ALIGN | DELETE;
        }
    if (DEBUG_DIR_M) {
        /* Print the alignment matrix */
        if (INSERT & dirMtx[pos])
            printf ("I");
        if (DELETE & dirMtx[pos])
            printf ("D");
        if (ALIGN & dirMtx[pos])
            printf ("A");
        printf ("\t");
    }
    if (DEBUG_COST_M) {
        /* Print the cost matrix */
        printf ("%" cost_p "\t", curRow[pos]);
        fflush (stdout);
    }
}

static inline void
algn_fill_last_column (       cost_t    *curRow
                      , const cost_t    *prevRow
                      ,       cost_t     const_val_tail
                      ,       size_t           lastColumnIndex
                      ,       DIR_MTX_ARROW_t *dirMtx
                      )
{
    cost_t cost;
    if (lastColumnIndex > 0) {
        cost = prevRow[lastColumnIndex] + const_val_tail; // Gotta add some tender loving care to the cost!
        if (cost < curRow[lastColumnIndex]) {
        curRow[lastColumnIndex] = cost;
            dirMtx[lastColumnIndex] = DELETE;
        }
        else if (cost == curRow[lastColumnIndex])
        dirMtx[lastColumnIndex] = dirMtx[lastColumnIndex] | DELETE;
    }
}

static inline void
algn_fill_full_row (       cost_t    *curRow
                   , const cost_t    *prevRow
                   , const cost_t    *gap_row
                   , const cost_t    *align_row
                   ,       DIR_MTX_ARROW_t *dirMtx
                   ,       int              const_val
                   ,       int              const_val_tail
                   ,       size_t           rowLength
                   )
{
    /* first entry is delete */
    curRow[0] = const_val + prevRow[0];
    dirMtx[0] = DELETE;

    if (DEBUG_DIR_M) {
        printf ("D\t");
    }

    algn_fill_row ( curRow
                  , prevRow
                  , gap_row
                  , align_row
                  , dirMtx
                  , const_val
                  , 1
                  , rowLength - 1
                  );

    algn_fill_last_column ( curRow
                          , prevRow
                          , const_val_tail
                          , rowLength - 1
                          , dirMtx
                          );

}

void
algn_fill_first_row ( cost_t          *curRow
                    , DIR_MTX_ARROW_t       *dirMtx
                    , size_t                 len
                    , cost_t    const *gap_row
                    )
{
    /* We fill the first cell to start with */
    curRow[0] = 0;
    dirMtx[0] = ALIGN;
    /* Now the rest of the row */
    if (DEBUG_DIR_M)
        printf ("A\t");
    if (DEBUG_DIR_M) {
        printf ("%" cost_p "\t", curRow[0]);
        fflush (stdout);
    }
    for (size_t i = 1; i < len; i++) {
        curRow[i] = curRow[i - 1] + gap_row[i];
        dirMtx[i] = INSERT;
        if (DEBUG_DIR_M)
            printf ("I\t");
        if (DEBUG_DIR_M) {
            printf ("%" cost_p "\t", curRow[i]);
            fflush (stdout);
        }
    }
}

void
algn_fill_first_cell ( cost_t    *curRow
                     , cost_t     prevRow
                     , DIR_MTX_ARROW_t *dirMtx
                     , elem_t           gap_char
                     )
{
    *curRow = prevRow + gap_char;
    *dirMtx = DELETE;
    if (DEBUG_DIR_M)
        printf ("D\t");
    if (DEBUG_DIR_M) {
        printf ("%" cost_p "\t", *curRow);
        fflush (stdout);
    }
}

/* In the following three functions, we maintain the following invariants in
 * each loop:
 * 1. curRow is a row that has not been filled and is the next to be.
 * 4. dirMtx is the current row of the direction matrix
 * 2. prevRow is the previous row, located right above curRow, which has been filled
 *    already.
 * 3. i is the number of the row of curRow in its containing matrix
 * 5. gap_row is the cost of aligning each base of char2 with a gap. This is
 *    constant for all the loop.
 * 6. cur_char1 is the i'th base of char1
 * 7. const_val is the cost of cur_char1 aligned with a gap
 * 8. align_row is the vector of costs of aligning char2 with cur_char1
 */

static inline cost_t *
algn_fill_extending_right ( const dyn_character_t   *char1
                          ,       cost_t      *algn_precalcMtx
                         // , size_t char1_len
                          ,       size_t              char2_len
                          ,       cost_t       *curRow
                          ,       cost_t       *prevRow
                          ,       DIR_MTX_ARROW_t    *dirMtx
                          , const cost_matrices_2d_t *costMatrix
                          ,       size_t              start_row
                          ,       size_t              end_row
                          ,       size_t              len
                          )
{
    // printf("algn_fill_extending_right\n");
    size_t i = start_row;
    cost_t *tmp, const_val;
    elem_t cur_char1;
    const cost_t *gap_row, *align_row;
    /** Invariants block
     * len is the number of items in the row to be filled **/

    /* This is what we will perform conceptually, I will stop using the
     * algnMtx_get_precal_row function to speed this up a little bit
    gap_row = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
    */
    gap_row = algn_precalcMtx + (costMatrix->gap_char * char2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_char1 = char1->char_begin[i];
        const_val = cm_calc_cost_2d( costMatrix->cost
                                   , cur_char1
                                   , costMatrix->gap_char
                                   , costMatrix->alphSize
                                   );
        /* This is conceptually what we do in the next line
        align_row = algnMtx_get_precal_row (algn_precalcMtx, cur_char1, char2_len);
        */
        align_row = algn_precalcMtx + (cur_char1 * char2_len);
        /* Align! */
        algn_fill_first_cell( curRow
                            , prevRow[0]
                            , dirMtx
                            , align_row[0]
                            );
        algn_fill_row( curRow
                     , prevRow
                     , gap_row
                     , align_row
                     , dirMtx
                     , const_val
                     , 1
                     , len - 2
                     );
        algn_fill_ukk_right_cell( curRow
                                , prevRow
                                , gap_row
                                , align_row
                                , dirMtx
                               // , const_val
                                , len - 1
                                );
        /** Invariants block */
        tmp     = curRow;
        curRow  = prevRow;
        prevRow = tmp;
        i++;
        dirMtx += char2_len;
        len++;
    }

    return (curRow);
}

static inline cost_t *
algn_fill_extending_left_right ( const dyn_character_t    *char1
                               ,       cost_t       *algn_precalcMtx
                              // , size_t char1_len
                               ,       size_t              char2_len
                               ,       cost_t       *curRow
                               ,       cost_t       *prevRow
                               ,       DIR_MTX_ARROW_t    *dirMtx
                               , const cost_matrices_2d_t *costMtx
                               ,       size_t              start_row
                               ,       size_t              end_row
                               ,       size_t              start_column
                               ,       size_t              len
                               )
{
    // printf("algn_fill_extending_left_right\n");
    size_t i;
    cost_t *tmpRow, const_val;
    elem_t cur_char1;
    const cost_t *gap_row, *align_row;
    /** Invariants block
     * len is the number of cells to fill in the current row minus 1
     * start_column is the first cell to fill in the row */
    i = start_row;
    /* Conceptually,
       gap_row = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
    */
    gap_row = algn_precalcMtx + (costMtx->gap_char * char2_len);
    len--;
    while (i < end_row) {
        /** Invariants block */
        cur_char1  = char1->char_begin[i];
        const_val = cm_calc_cost_2d( costMtx->cost
                                   , cur_char1
                                   , costMtx->gap_char
                                   , costMtx->alphSize
                                   );
        /* Conceptually,
           align_row = algnMtx_get_precal_row (algn_precalcMtx, cur_char1, char2_len);
        */
        align_row = algn_precalcMtx + (cur_char1 * char2_len);
        /* Align! */
        algn_fill_ukk_left_cell( curRow
                               , prevRow
                              // , gap_row
                               , align_row
                               , dirMtx
                               , const_val
                               , start_column
                               );
        algn_fill_row( curRow
                     , prevRow
                     , gap_row
                     , align_row
                     , dirMtx
                     , const_val
                     , start_column + 1
                     , start_column + (len - 2)
                     );
        algn_fill_ukk_right_cell( curRow
                                , prevRow
                                , gap_row
                                , align_row
                                , dirMtx
                               // , const_val
                                , start_column + len - 1
                                );
        /** Invariants block */
        tmpRow  = curRow;
        curRow  = prevRow;
        prevRow = tmpRow;
        i++;
        dirMtx += char2_len;
        start_column++;
    }
    return (curRow);
}

static inline cost_t *
algn_fill_extending_left ( const dyn_character_t    *char1
                         ,       cost_t       *algn_precalcMtx
                        // , size_t char1_len
                         ,       size_t              char2_len
                         ,       cost_t       *curRow
                         ,       cost_t       *prevRow
                         ,       DIR_MTX_ARROW_t    *dirMtx
                         , const cost_matrices_2d_t *costMatrix
                         ,       size_t              start_row
                         ,       size_t              end_row
                         ,       size_t              start_column     // the first cell to fill in the row
                         ,       size_t              len              // len is the number of cells to fill in the current row minus 1
                         )
{
    if (DEBUG_CALL_ORDER) printf("algn_fill_extending_left\n");
    size_t i = start_row;
    cost_t *tmpRow,
                  const_val,
                  const_val_tail;

    elem_t cur_char1;

    const cost_t *gap_row,
                       *align_row;

    /* Conceptually,
       gap_row = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
    */
    gap_row = algn_precalcMtx + (costMatrix->gap_char * char2_len);
    while (i < end_row) {
        /** Invariants block */
        cur_char1       = char1->char_begin[i];
        const_val      = cm_calc_cost_2d ( costMatrix->cost
                                         , cur_char1
                                         , costMatrix->gap_char
                                         , costMatrix->alphSize
                                         );

        const_val_tail = costMatrix->tail_cost[cur_char1];

        /* Conceptually,
           align_row = algnMtx_get_precal_row (algn_precalcMtx, cur_char1, char2_len);
        */
        align_row = algn_precalcMtx + (cur_char1 * char2_len);
        /* Align! */
        algn_fill_ukk_left_cell ( curRow
                                , prevRow
                               // , gap_row
                                , align_row
                                , dirMtx
                                , const_val
                                , start_column
                                );

        algn_fill_row ( curRow
                      , prevRow
                      , gap_row
                      , align_row
                      , dirMtx
                      , const_val
                      , start_column + 1
                      , start_column + len - 1
                      );

        algn_fill_last_column ( curRow
                              , prevRow
                              , const_val_tail
                              , start_column + len - 1
                              , dirMtx
                              );

        /** Invariants block */
        tmpRow  = curRow;
        curRow  = prevRow;
        prevRow = tmpRow;
        i++;
        dirMtx += char2_len;
        start_column++;
        len--;
    }
    if (DEBUG_COST_M) {
        printf ("A_A_A gap cost\n");
        fflush (stdout);
        for (i = 0; i < char2_len; i++) {
            printf ("%" cost_p "\t", gap_row[i]);
            fflush (stdout);
        }
        printf ("\n");
        printf ("The G_A_A - gap cost is %" cost_p "\n", const_val);
        fflush (stdout);
    }

    return (curRow);
}

static inline cost_t *
algn_fill_no_extending ( const dyn_character_t    *char1
                       ,       cost_t       *algn_precalcMtx
                      // ,       int  char1_len
                       ,       cost_t        char2_len
                       ,       cost_t       *curRow
                       ,       cost_t       *prevRow
                       ,       DIR_MTX_ARROW_t    *dirMtx
                       , const cost_matrices_2d_t *costMatrix
                       ,       size_t              start_row
                       ,       size_t              end_row
                       )
{
    // printf("algn_fill_no_extending\n");
    size_t i;
    cost_t *tmpRow,
                  const_val,
                  const_val_tail;

    size_t cur_char1;

    cost_t *gap_row,
                 *align_row;

    /** Invariants block */
    i = start_row;
    /* Conceptually,
       gap_row = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
    */
    gap_row = algn_precalcMtx + (costMatrix->gap_char * char2_len);

    while (i < end_row) {
        /** Invariants block */
        cur_char1      = char1->char_begin[i];
        const_val      = cm_calc_cost_2d ( costMatrix->cost
                                         , cur_char1
                                         , costMatrix->gap_char
                                         , costMatrix->alphSize
                                         );

        const_val_tail = costMatrix->tail_cost[cur_char1];
        /* Conceptually,
           align_row = algnMtx_get_precal_row (algn_precalcMtx, cur_char1, char2_len);
        */
        align_row = algn_precalcMtx + (cur_char1 * char2_len);

        /* Align! */
        algn_fill_first_cell ( curRow
                             , prevRow[0]
                             , dirMtx
                             , align_row[0]
                             );

        algn_fill_row ( curRow
                      , prevRow
                      , gap_row
                      , align_row
                      , dirMtx
                      , const_val
                      , 1
                      , char2_len - 1
                      );

        algn_fill_last_column ( curRow
                              , prevRow
                              , const_val_tail
                              , char2_len - 1
                              , dirMtx
                              );

        /** Invariants block */
        tmpRow  = curRow;
        curRow  = prevRow;
        prevRow = tmpRow;
        dirMtx += char2_len;
        i++;
    }

    return curRow;
}

/* Similar to the previous but when no barriers are set */
static inline cost_t
algn_fill_plane ( const dyn_character_t    *longerCharacter
                ,       cost_t       *algn_precalcMtx
                 // Leading gap included in input lengths
                ,       size_t              longerCharacterLength // larger, horizontal dimension
                ,       size_t              lesserCharacterLength // smaller, vertical dimension
                ,       cost_t       *curRow
                ,       DIR_MTX_ARROW_t    *dirMtx
                , const cost_matrices_2d_t *costMatrix
                )
{
    // printf("algn_fill_plane\n");
    // printf("lesserCharacterLength: %d\n", lesserCharacterLength);
    // printf("longerCharacterLength: %d\n", longerCharacterLength);

    size_t i, j;
    const cost_t *align_row,
                       *gap_row,
                       *first_gap_row;

    cost_t  const_val,
                  const_val_tail,
                 *prevRow , // This is misnamed, it is a buffer reference to the previous row.
                 *tmpRow,
                  gapcode,
                  curChar1_elem;

    int* debugCostMatrixBuffer = NULL; // Only used in DEBUG_COST_M branches

    // change to 1 || DEBUG_COST_M to just print the cost matrix in here.
    const int LOCAL_DEBUG_COST_M = DEBUG_COST_M;

    /* A precalculated cost of a gap aligned with each base in the array */
    gapcode       = costMatrix->gap_char;
    gap_row       = algnMtx_get_precal_row (algn_precalcMtx, gapcode, lesserCharacterLength);
    first_gap_row = algnMtx_get_precal_row (algn_precalcMtx,       0, lesserCharacterLength);
    prevRow       = curRow;
    curRow[0]     = 0;
    dirMtx[0]     = ALIGN;

    if (LOCAL_DEBUG_COST_M) {
        //Allocate space to store cost matrix proper as it is continually overwritten in the algorithm below.
        debugCostMatrixBuffer = malloc(longerCharacterLength * lesserCharacterLength * sizeof(int));
        assert( debugCostMatrixBuffer != NULL && "OOM allocing debug cost matrix buffer." );
    }
    if (DEBUG_DIR_M) {
        printf ("A\t");
    }

    /* We fill the first row to start with */
    for (i = 1; i < lesserCharacterLength; i++) {
        curRow[i] = curRow[i - 1] + first_gap_row[i];
        dirMtx[i] = INSERT;
        if (DEBUG_DIR_M) {
            printf ("I\t");
        }
    }
    if (DEBUG_DIR_M) {
        printf ("\n");
    }

    if (LOCAL_DEBUG_COST_M) {
        for (i = 0; i < lesserCharacterLength; i++) {
            debugCostMatrixBuffer[i] = curRow[i];
        }
    }

    curRow += lesserCharacterLength;


    /* Now we fill the rest of the matrix */
    for ( i = 1, dirMtx += lesserCharacterLength
        ; i < longerCharacterLength
        ; i++, dirMtx += lesserCharacterLength
        ) {

        curChar1_elem  = longerCharacter->char_begin[i];
        const_val_tail = costMatrix->tail_cost[curChar1_elem]; // get tail cost in costMatrix for value at
                                                               // position i in char1
        // printf("const_val_tail: %d\n",const_val_tail);
        const_val = cm_calc_cost_2d ( costMatrix->cost
                                    , longerCharacter->char_begin[i]
                                    , costMatrix->gap_char
                                    , costMatrix->alphSize
                                    );

        align_row = algnMtx_get_precal_row ( algn_precalcMtx
                                           , longerCharacter->char_begin[i]
                                           , lesserCharacterLength
                                           );

        algn_fill_full_row ( curRow
                           , prevRow
                           , gap_row
                           , align_row
                           , dirMtx
                           , const_val
                           , const_val_tail
                           , lesserCharacterLength
                           );

        if (LOCAL_DEBUG_COST_M) {
            for (j = 0; j < lesserCharacterLength; j++) {
                debugCostMatrixBuffer[(lesserCharacterLength * i) + j] = curRow[j];
            }
        }

        /* We swap curRow and prevRow  for the next round */
        tmpRow  = curRow;
        curRow  = prevRow ;
        prevRow = tmpRow;
    }

    if (LOCAL_DEBUG_COST_M) {
        printf("Cost matrix:\n");

        // Print cost matrix column headers
        printf("  x |    * ");
        for (i = 1; i < longerCharacterLength; i++) {
            printf("%4" elem_p " ", longerCharacter->char_begin[i]);
        }
        printf("\n");
        printf(" ---+-");

        for (i = 0; i < longerCharacterLength; i++) {
            printf("-----");
        }
        printf("\n");

    // Print cost matrix rows
        for (i = 0; i < lesserCharacterLength; i++) {

        // Print cost matrix row header
            if (i == 0) printf ("  * | ");
            else        printf ("  # | "); // Character not in scope!

            for (j = 0; j < longerCharacterLength; j++) {
                printf ("%4" elem_p " ", debugCostMatrixBuffer[lesserCharacterLength * j + i]);
            }
            printf ("\n");
        }
        free(debugCostMatrixBuffer);
    }

    return prevRow[lesserCharacterLength - 1];
}

static inline cost_t *
choose_other ( cost_t *compare
             , cost_t *a
             , cost_t *b) {
    if (a == compare) {
        return b;
    } else {
        return a;
    }
}

static inline int
algn_fill_plane_2 ( const dyn_character_t    *longerChar
                  ,       cost_t       *algn_precalcMtx
                  ,       size_t              longerChar_len
                  ,       size_t              len_lesserChar
                  ,       cost_t       *curRow
                  ,       DIR_MTX_ARROW_t    *dirMtx
                  , const cost_matrices_2d_t *costMatrix
                  ,       size_t              width
                  ,       size_t              height
                  ,       size_t              deltawh
                  )
{
    // printf("algn_fill_plane_2 %d", iteration);
    //fflush(stdout);
    cost_t *next_row,
                 *next_prevRow,
                 *prevRow,
                  start_row,
                  final_row,
                  start_column,
                  length;

    cost_t const *gap_row;

    DIR_MTX_ARROW_t *to_go_dirMtx;
    width = width + deltawh;

    if (width > len_lesserChar) {
        width = len_lesserChar; // width is at most len_lesserChar
    }

    height = height + deltawh;

    if (height > longerChar_len) {
        height = longerChar_len; // likewise, height is at most longerChar_len
    }

    prevRow = curRow + len_lesserChar;
    gap_row = algnMtx_get_precal_row( algn_precalcMtx
                               , 0
                               , len_lesserChar // We want the first horizontal row
                               );

    /* We have to consider three cases in this new alignment procedure (much
     * cleaner than the previous):
     *
     * Case 1:
     * If longerChar_len >= 1.5 * char2_len, there is no point in using the
     * barriers. Rather, we fill the full matrix in one shot */
    if (longerChar_len >= 1.5 * len_lesserChar) { // deleted a bunch of casts here
        return algn_fill_plane ( longerChar
                               , algn_precalcMtx
                               , longerChar_len
                               , len_lesserChar
                               , curRow
                               , dirMtx
                               , costMatrix
                               );
    }
    /* Case 2:
     * There are no full rows to be filled, therefore we have to break the
     * procedure into two different subsets */
    // subset 1:
    else if (2 * height < longerChar_len) {
        algn_fill_first_row (curRow, dirMtx, width, gap_row);
        start_row    = 1;
        final_row    = height;
        start_column = 0;
        length       = width + 1;
        to_go_dirMtx = dirMtx + (start_row * len_lesserChar);

        /* Now we fill that space */
        next_row = algn_fill_extending_right ( longerChar
                                             , algn_precalcMtx
                                            // , longerChar_len
                                             , len_lesserChar
                                             , prevRow
                                             , curRow
                                             , to_go_dirMtx
                                             , costMatrix
                                             , start_row
                                             , final_row
                                             , length
                                             );

        next_prevRow = choose_other (next_row, curRow, prevRow);
        /* Next group */
        start_row    = final_row;
        final_row    = longerChar_len - (height - 1);
        start_column = 1;
        length       = width  + height;
        to_go_dirMtx = dirMtx + (start_row * len_lesserChar);

        next_row     = algn_fill_extending_left_right ( longerChar
                                                      , algn_precalcMtx
                                                     // , longerChar_len
                                                      , len_lesserChar
                                                      , next_row
                                                      , next_prevRow
                                                      , to_go_dirMtx
                                                      , costMatrix
                                                      , start_row
                                                      , final_row
                                                      , start_column
                                                      , length
                                                      );

        next_prevRow = choose_other (next_row, curRow, prevRow);
        /* The final group */
        start_row    = final_row;
        final_row    = longerChar_len;
        length       = length - 2;
        start_column = len_lesserChar - length;
        to_go_dirMtx = dirMtx + (start_row * len_lesserChar);

        next_row = algn_fill_extending_left ( longerChar
                                            , algn_precalcMtx
                                           // , longerChar_len
                                            , len_lesserChar
                                            , next_row
                                            , next_prevRow
                                            , to_go_dirMtx
                                            , costMatrix
                                            , start_row
                                            , final_row
                                            , start_column
                                            , length
                                            );

        next_prevRow = choose_other (next_row, curRow, prevRow);
    }
    /* Case 3: (final case)
     * There is a block in the middle with full rows that have to be filled
     */
    else {
        /* We will simplify this case even further: If the size of the leftover
         * is too small, don't use the barriers at all, just fill it all up
         */
        // subset 2:
        if (8 >= (longerChar_len - height)) {
            return (algn_fill_plane ( longerChar
                                    , algn_precalcMtx
                                    , longerChar_len
                                    , len_lesserChar
                                    , curRow
                                    , dirMtx
                                    , costMatrix
                                    )
                    );
        // subset 3:
        } else {
            algn_fill_first_row (curRow, dirMtx, width, gap_row);
            start_row    = 1;
            final_row    = (len_lesserChar - width) + 1;
            start_column = 0;
            length       = width + 1;
            to_go_dirMtx = dirMtx + (len_lesserChar * start_row);
            next_row     = algn_fill_extending_right ( longerChar
                                                     , algn_precalcMtx
                                                    // , longerChar_len
                                                     , len_lesserChar
                                                     , prevRow
                                                     , curRow
                                                     , to_go_dirMtx
                                                     , costMatrix
                                                     , start_row
                                                     , final_row
                                                     , length
                                                     );

            next_prevRow = choose_other (next_row, curRow, prevRow);
            start_row    = final_row;
            final_row    = longerChar_len - (len_lesserChar - width) + 1;
            length       = len_lesserChar;
            to_go_dirMtx = dirMtx + (len_lesserChar * start_row);
            next_row     = algn_fill_no_extending ( longerChar
                                                  , algn_precalcMtx
                                                 // , longerChar_len
                                                  , len_lesserChar
                                                  , next_row
                                                  , next_prevRow
                                                  , to_go_dirMtx
                                                  , costMatrix
                                                  , start_row
                                                  , final_row
                                                  );

            next_prevRow = choose_other (next_row, curRow, prevRow);
            start_row    = final_row;
            final_row    = longerChar_len;
            start_column = 1;
            length       = len_lesserChar - 1;
            to_go_dirMtx = dirMtx + (len_lesserChar * start_row);
            next_row     = algn_fill_extending_left ( longerChar
                                                    , algn_precalcMtx
                                                   // , longerChar_len
                                                    , len_lesserChar
                                                    , next_row
                                                    , next_prevRow
                                                    , to_go_dirMtx
                                                    , costMatrix
                                                    , start_row
                                                    , final_row
                                                    , start_column
                                                    , length
                                                    );

            next_prevRow = choose_other (next_row, curRow, prevRow);
        }
    }
    return next_prevRow[len_lesserChar - 1];
}
/******************************************************************************/

/******************************************************************************/
/*                         Pairwise Affine Alignment                          */
/******************************************************************************/
/*
 *
 * WARNING! This is a copy of the pairwise standard alignment, modified slightly
 * for the affine case. This is for performance issues! so beware, any change
 * here must also go there.
 */

#define algn_assign_dirMtx(dirMtx, pos, v) dirMtx[pos] = dirMtx[pos] | v

static inline void
algn_fill_row_affine (       cost_t    *curRow
                     , const cost_t    *prevRow
                     , const cost_t    *gap_row
                     , const cost_t    *align_row
                     ,       DIR_MTX_ARROW_t *dirMtx
                     ,       cost_t     const_val
                     ,       cost_t     const_val_prev
                     ,       size_t           st
                     ,       size_t           end
                     ,       cost_t    *dncurRow
                     , const cost_t    *pdncurRow
                     ,       cost_t    *htcurRow
                     ,       cost_t     gap_open_cost
                     )
{

    size_t i;
    int tmp1, tmp2, tmp3, tmp4, tmp5;

    for (i = st; i <= end; i++) {
        /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS
        assert ((curRow    + i) < _algn_max_matrix);
        assert ((prevRow   + i) < _algn_max_matrix);
        assert ((dirMtx    + i) < _algn_max_direction);
        assert ((dncurRow  + i) < _algn_max_matrix);
        assert ((pdncurRow + i) < _algn_max_matrix);
        assert ((htcurRow  + i) < _algn_max_matrix);
#endif
        dirMtx[i] = 0;
        { /* We deal with the difficulty of using an opening gap as
             another DIR_MTX_ARROW_t */
            if ((0 == const_val_prev) && (0 != const_val)) {
                tmp1 = pdncurRow[i] + gap_open_cost + const_val;
                tmp4 = prevRow[i]   + gap_open_cost + const_val;
            }
            else if ((0 != const_val_prev) && (0 == const_val)) {
                tmp1 = pdncurRow[i] + gap_open_cost + const_val;
                tmp4 = prevRow[i];
            }
            else {
                tmp1 = pdncurRow[i] + const_val;
                tmp4 = prevRow[i]   + gap_open_cost + const_val;
            }

            if ((0 == gap_row[i - 1]) && (0 != gap_row[i])) {
                tmp5 = htcurRow[i - 1] + gap_open_cost + gap_row[i];
                tmp2 = curRow[i - 1]   + gap_open_cost + gap_row[i];
            }
            else if ((0 != gap_row[i - 1]) && (0 == gap_row[i])) {
                tmp5 = htcurRow[i - 1] + gap_open_cost + gap_row[i];
                tmp2 = curRow[i - 1];
            }
            else {
                tmp2 = curRow[i - 1]   + gap_open_cost + gap_row[i];
                tmp5 = htcurRow[i - 1] + gap_row[i];
            }

            if ((((0 == gap_row[i]) && (0 != const_val)) ||
                    ((0 != gap_row[i]) && (0 == const_val))) &&
                    ((0 == gap_row[i - 1]) || (0 == const_val_prev)))
                tmp3 = prevRow[i - 1] + gap_open_cost + align_row[i];
            else
                tmp3 = prevRow[i - 1] + align_row[i];
        }
        if (tmp1 < tmp4)
            algn_assign_dirMtx(dirMtx, i, DELETE_V);
        else {
            algn_assign_dirMtx(dirMtx, i, ALIGN_V);
            tmp1 = tmp4;
        }

        if (tmp2 <= tmp5) {
            algn_assign_dirMtx(dirMtx, i, ALIGN_H);
        }
        else {
            tmp2 = tmp5;
            algn_assign_dirMtx(dirMtx, i, INSERT_H);
        }
        dncurRow[i] = tmp1;
        htcurRow[i] = tmp2;

        /* check whether insertion is better */
        /* This option will allow all the possible optimal paths to be stored
         * concurrently on the same backtrace matrix. This is important for the
         * characters being able to choose the appropriate direction while
         * keeping the algorithm that assumes that char2 is at most as long as char1.
         * */
        if (tmp1 < tmp3) {
            if (tmp1 < tmp2) {
                curRow[i] = tmp1;
                algn_assign_dirMtx(dirMtx, i, DELETE);
            }
            else if (tmp2 < tmp1) {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, INSERT);
            }
            else {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, (DELETE | INSERT));
            }
        }
        else if (tmp3 < tmp1) {
            if (tmp3 < tmp2) {
                curRow[i] = tmp3;
                algn_assign_dirMtx(dirMtx, i, ALIGN);
            }
            else if (tmp2 < tmp3) {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, INSERT);
            }
            else {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, ALIGN | INSERT);
            }
        }
        else { /* tmp3 == tmp1 */
            if (tmp3 < tmp2) {
                curRow[i] = tmp3;
                algn_assign_dirMtx(dirMtx, i, ALIGN | DELETE);
            }
            else if (tmp2 < tmp3) {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, INSERT);
            }
            else {
                curRow[i] = tmp2;
                algn_assign_dirMtx(dirMtx, i, DELETE | INSERT | ALIGN);
            }
        }
        if (DEBUG_DIR_M) {
            /* Print the alignment matrix */
            if (INSERT & dirMtx[i])
                printf ("I");
            if (DELETE & dirMtx[i])
                printf ("D");
            if (ALIGN & dirMtx[i])
                printf ("A");
            printf ("\t");
        }
        if (DEBUG_COST_M) {
            /* Print the cost matrix */
            printf ("(%" cost_p ", %" cost_p ", %" cost_p ")\t", curRow[i], htcurRow[i], dncurRow[i]);
            fflush (stdout);
        }
    }
    if (DEBUG_DIR_M) {
        printf ("\n");
        fflush (stdout);
    }
}

static inline void
algn_fill_ukk_right_cell_affine (       cost_t    *curRow
                                , const cost_t    *prevRow
                                , const cost_t    *gap_row
                                , const cost_t    *align_row
                                ,       DIR_MTX_ARROW_t *dirMtx
                                ,       cost_t     const_val
                                ,       cost_t     const_val_prev
                                ,       cost_t     pos
                                ,       cost_t    *dncurRow
                                ,       cost_t    *htcurRow
                                ,       cost_t     gap_open_cost
                                )
{
    int tmp2, tmp3, tmp4;
    /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS
        assert ((curRow   + pos) < _algn_max_matrix);
        assert ((prevRow  + pos) < _algn_max_matrix);
        assert ((dirMtx   + pos) < _algn_max_direction);
        assert ((htcurRow + pos) < _algn_max_matrix);
#endif
    dirMtx[pos] = 0;
    { /* Affine gap difficultness */
        if ((0 != gap_row[pos - 1]) && (0 == gap_row[pos]))
            tmp2 = curRow[pos - 1];
        else
            tmp2 = curRow[pos - 1] + gap_open_cost + gap_row[pos];

        if (((0 == gap_row[pos - 1]) && (0 != gap_row[pos])) ||
            ((0 != gap_row[pos - 1]) && (0 == gap_row[pos])))
            tmp4 = htcurRow[pos - 1] + gap_open_cost + gap_row[pos];
        else
            tmp4 = htcurRow[pos - 1] + gap_row[pos];

        if ((((0 == gap_row[pos]) && (0 != const_val)) ||
                ((0 != gap_row[pos]) && (0 == const_val))) &&
                ((0 == gap_row[pos - 1]) || (0 == const_val_prev)))
            tmp3 = prevRow[pos - 1] + gap_open_cost + align_row[pos];
        else
            tmp3 = prevRow[pos - 1] + align_row[pos];
    }
    if (tmp2 <= tmp4)
        algn_assign_dirMtx(dirMtx, pos, ALIGN_H);
    else {
        tmp2 = tmp4;
        algn_assign_dirMtx(dirMtx, pos, INSERT_H);
    }
    htcurRow[pos] = tmp2;
    dncurRow[pos] = VERY_LARGE_NUMBER;
    /* check whether insertion is better */
    if (tmp2 < tmp3) {
        curRow[pos] = tmp2;
        algn_assign_dirMtx(dirMtx, pos, (INSERT));
    }
    else if (tmp3 < tmp2) {
        curRow[pos] = tmp3;
        algn_assign_dirMtx(dirMtx, pos, (ALIGN));
    }
    else {
        curRow[pos] = tmp3;
        algn_assign_dirMtx(dirMtx, pos, (INSERT | ALIGN));
    }
    if (DEBUG_DIR_M) {
        /* Print the alignment matrix */
        if (INSERT & dirMtx[pos])
            printf ("I");
        if (DELETE & dirMtx[pos])
            printf ("D");
        if (ALIGN & dirMtx[pos])
            printf ("A");
        printf ("\t");
    }
    if (DEBUG_COST_M) {
        /* Print the cost matrix */
        printf ("(%" cost_p ", %" cost_p ")\t", curRow[pos], htcurRow[pos]);
    }
    if (DEBUG_DIR_M || DEBUG_COST_M)
        printf ("\n");
}

static inline void
algn_fill_ukk_left_cell_affine (       cost_t    *curRow
                               , const cost_t    *prevRow
                               , const cost_t    *gap_row
                               , const cost_t    *align_row
                               ,       DIR_MTX_ARROW_t *dirMtx
                               ,       cost_t     const_val
                               ,       cost_t     const_val_prev
                               ,       size_t           pos
                               ,       cost_t    *dncurRow
                               ,       cost_t    *pdncurRow
                               ,       cost_t    *htcurRow
                               ,       cost_t     gap_open_cost
                               )
{
    /* try align with substitution */
#ifdef DEBUG_ALL_ASSERTIONS
        assert ((curRow    + pos) < _algn_max_matrix);
        assert ((prevRow   + pos) < _algn_max_matrix);
        assert ((dirMtx    + pos) < _algn_max_direction);
        assert ((dncurRow  + pos) < _algn_max_matrix);
        assert ((pdncurRow + pos) < _algn_max_matrix);
#endif

    cost_t tmp1,
                 tmp3,
                 tmp5;

    dirMtx[pos] = 0;
    { /* Affine gap difficultness */
        if ((0 != const_val_prev) && (0 == const_val))
            tmp1 = prevRow[pos];
        else
            tmp1 = prevRow[pos] + gap_open_cost + const_val;

        if (((0 == const_val_prev) && (0 != const_val)) ||
            ((0 != const_val_prev) && (0 == const_val)))
            tmp5 = pdncurRow[pos] + gap_open_cost + const_val;
        else
            tmp5 = pdncurRow[pos] + const_val;

        if ((((0 == gap_row[pos]) && (0 != const_val)) ||
                ((0 != gap_row[pos]) && (0 == const_val))) &&
                    ((0 == gap_row[pos - 1]) || (0 == const_val_prev)))
            tmp3 = prevRow[pos - 1] + gap_open_cost + align_row[pos];
        else
            tmp3 = prevRow[pos - 1] + align_row[pos];
    }
    if (tmp1 <= tmp5)
        algn_assign_dirMtx(dirMtx, pos, ALIGN_V);
    if (tmp5 < tmp1) {
        algn_assign_dirMtx(dirMtx, pos, DELETE_V);
        tmp1 = tmp5;
    }
    dncurRow[pos] = tmp1;
    htcurRow[pos] = VERY_LARGE_NUMBER;
        if (tmp1 < tmp3) {
            curRow[pos] = tmp1;
            algn_assign_dirMtx(dirMtx, pos, (DELETE));
        }
        else if (tmp3 < tmp1) {
            curRow[pos] = tmp3;
            algn_assign_dirMtx(dirMtx, pos, (ALIGN));
        }
        else {
            curRow[pos] = tmp1;
            algn_assign_dirMtx(dirMtx, pos, (ALIGN | DELETE));
        }
    if (DEBUG_DIR_M) {
        /* Print the alignment matrix */
        if (INSERT & dirMtx[pos])
            printf ("I");
        if (DELETE & dirMtx[pos])
            printf ("D");
        if (ALIGN & dirMtx[pos])
            printf ("A");
        printf ("\t");
    }
    if (DEBUG_COST_M) {
        /* Print the cost matrix */
        printf ("(%" cost_p ", ,%" cost_p ")\t", curRow[pos], dncurRow[pos]);
    }
}

static inline void
algn_fill_last_column_affine (       cost_t    *curRow
                             , const cost_t    *prevRow
                             ,       cost_t     const_val_tail
                             ,       int              const_val_tail_prev
                             ,       size_t           length
                             ,       DIR_MTX_ARROW_t *dirMtx
                             ,       cost_t    *dncurRow
                             , const cost_t    *pdncurRow
                             ,       cost_t     gap_open_cost
                             )
{
    cost_t cst, tmp2;

#ifdef DEBUG_ALL_ASSERTIONS
        assert ((curRow    + length) < _algn_max_matrix);
        assert ((prevRow   + length) < _algn_max_matrix);
        assert ((dirMtx    + length) < _algn_max_direction);
        assert ((dncurRow  + length) < _algn_max_matrix);
        assert ((pdncurRow + length) < _algn_max_matrix);
#endif

    tmp2 = prevRow[length] + const_val_tail + gap_open_cost;
    { /* Affine gap difficultness */
        if (((0 == const_val_tail_prev) && (0 != const_val_tail)) ||
            ((0 != const_val_tail_prev) && (0 == const_val_tail)))
            cst = pdncurRow[length] + gap_open_cost + const_val_tail;
        else
            cst = pdncurRow[length] + const_val_tail;
    }
    if (cst < tmp2)
        algn_assign_dirMtx(dirMtx, length, DELETE_V);
    else {
        cst = tmp2;
        algn_assign_dirMtx(dirMtx, length, ALIGN_V);
    }
    dncurRow[length] = cst;
    if (cst < curRow[length]) {
        curRow[length] = cst;
        algn_assign_dirMtx(dirMtx, length, (DELETE));
    }
    else if (cst == curRow[length])
        algn_assign_dirMtx(dirMtx, length, DELETE);
}


static inline void
algn_fill_full_row_affine (       cost_t    *curRow
                          , const cost_t    *prevRow
                          , const cost_t    *gap_row
                          , const cost_t    *align_row
                          ,       DIR_MTX_ARROW_t *dirMtx
                          ,       cost_t     const_val
                          ,       cost_t     prev_const_val
                          ,       cost_t     const_val_tail
                          ,       cost_t     const_val_tail_prev
                          ,       size_t           char_length
                          ,       cost_t    *dncurRow
                          , const cost_t    *pdncurRow
                          ,       cost_t    *htcurRow
                          ,       int              gap_open_cost
                          )
{
    /* first entry is delete */
    htcurRow[0] = VERY_LARGE_NUMBER;
    curRow[0]  += const_val;
    dirMtx[0]   = DELETE | DELETE_V;
    dncurRow[0] = const_val + pdncurRow[0];

    if (DEBUG_COST_M)
        printf ("%" cost_p "\t", curRow[0]);
    if (DEBUG_DIR_M)
        printf ("D\t");
    algn_fill_row_affine ( curRow
                         , prevRow
                         , gap_row
                         , align_row
                         , dirMtx
                         , const_val
                         , prev_const_val
                         , 1
                         , char_length - 1
                         , dncurRow
                         , pdncurRow
                         , htcurRow
                         , gap_open_cost
                         );

    algn_fill_last_column_affine ( curRow
                                 , prevRow
                                 , const_val_tail
                                 , const_val_tail_prev
                                 , char_length - 1
                                 , dirMtx
                                 , dncurRow
                                 , pdncurRow
                                 , gap_open_cost
                                 );
}

void
algn_fill_first_row_affine ( cost_t       *curRow
                           , DIR_MTX_ARROW_t    *dirMtx
                           , size_t              len
                           , cost_t const *gap_row
                           , cost_t       *dncurRow
                           , cost_t       *htcurRow
                           , cost_t        gap_open_cost
                           )
{
    /* We fill the first cell to start with */
    curRow[0]   = gap_open_cost;

    dncurRow[0] = htcurRow[0]
                = VERY_LARGE_NUMBER;

    dirMtx[0]   = ALIGN | ALIGN_V | ALIGN_H;

    /* Now the rest of the row */
    if (DEBUG_DIR_M)
        printf ("A\t");
    if (DEBUG_COST_M)
        printf ("%" cost_p "\t", curRow[0]);

    for (size_t i = 1; i < len; i++) {
        dncurRow[i] = VERY_LARGE_NUMBER;
        curRow[i]   = curRow[i - 1] + gap_row[i];
        dirMtx[i]   = INSERT | (INSERT_H);

        if (DEBUG_DIR_M)  printf ("I\t");
        if (DEBUG_COST_M) printf ("%" cost_p "\t", curRow[i]);
    }
}

void
algn_fill_first_cell_affine ( cost_t    *curRow
                            // , int              prevRow
                            , DIR_MTX_ARROW_t *dirMtx
                            , elem_t           gap_char
                            , cost_t    *dncurRow
                            , cost_t    *pdncurRow
                            , cost_t    *htcurRow
                            )
{
    htcurRow[0] = VERY_LARGE_NUMBER;
    curRow[0]  += gap_char;
    *dirMtx     = DELETE | DELETE_V;
    dncurRow[0] = gap_char + pdncurRow[0];
    if (DEBUG_DIR_M)
        printf ("D\t");
    if (DEBUG_COST_M)
        printf ("%" cost_p "\t", *curRow);
}

/* In the following three functions, we maintain the following invariants in
 * each loop:
 * 1. curRow is a row that has not been filled and is the next to be.
 * 4. dirMtx is the current row of the direction matrix
 * 2. prevRow is the previous row, located right above curRow, which has been filled
 *    already.
 * 3. i is the number of the row of curRow in its containing matrix
 * 5. gap_row is the cost of aligning each base of char2 with a gap. This is
 *    constant for all the loop.
 * 6. cur_char1 is the i'th base of char1
 * 7. const_val is the cost of cur_char1 aligned with a gap
 * 8. align_row is the vector of costs of aligning char2 with cur_char1
 */

static inline cost_t *
algn_fill_extending_right_affine ( const dyn_character_t     *char1
                                 ,       cost_t        *algn_precalcMtx
                                // ,       size_t char1_len
                                 ,       size_t               char2_len
                                 ,       cost_t        *curRow
                                 ,       cost_t        *prevRow
                                 ,       DIR_MTX_ARROW_t     *dirMtx
                                 , const cost_matrices_2d_t  *costMatrix
                                 ,       size_t               start_row
                                 ,       size_t               end_row
                                 ,       size_t               len
                                 ,       cost_t        *dncurRow
                                 ,       cost_t        *pdncurRow
                                 ,       cost_t        *htcurRow
                                 ,       cost_t         gap_open_cost
                                 )
{
    // printf("algn_fill_extending_right_affine\n");

    cost_t *tmp,
                 *tmp1,
                  cur_char1,
                  const_val,
                  prev_const_val,
                 *gap_row,
                 *align_row;

    elem_t prev_char1;

    /** Invariants block
     *  len is the number of items in the row to be filled
     */
    size_t i = start_row;

    /* This is what we will perform conceptually, I will stop using the
     * algnMtx_get_precal_row function to speed this up a little bit
     * gap_row = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
    */
    gap_row = algn_precalcMtx + (costMatrix->gap_char * char2_len);
    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_char1      = char1->char_begin[i - 1];
        cur_char1       = char1->char_begin[i];

        const_val      = cm_calc_cost_2d( costMatrix->cost
                                        , cur_char1
                                        , costMatrix->gap_char
                                        , costMatrix->alphSize
                                        );
        prev_const_val = cm_calc_cost_2d( costMatrix->cost
                                        , prev_char1
                                        , costMatrix->gap_char
                                        , costMatrix->alphSize
                                        );

        /* This is conceptually what we do in the next line
         * align_row = algnMtx_get_precal_row (algn_precalcMtx, cur_char1, char2_len);
         */
        align_row = algn_precalcMtx + (cur_char1 * char2_len);

        /* Align! */
        algn_fill_first_cell_affine( curRow
                                  // , prevRow[0]
                                   , dirMtx
                                   , align_row[0]
                                   , dncurRow
                                   , pdncurRow
                                   , htcurRow
                                   );

        algn_fill_row_affine( curRow
                            , prevRow
                            , gap_row
                            , align_row
                            , dirMtx
                            , const_val
                            , prev_const_val
                            , 1
                            , len - 2
                            , dncurRow
                            , pdncurRow
                            , htcurRow
                            , gap_open_cost
                            );

        algn_fill_ukk_right_cell_affine( curRow
                                       , prevRow
                                       , gap_row
                                       , align_row
                                       , dirMtx
                                       , const_val
                                       , prev_const_val
                                       , len - 1
                                       , dncurRow
                                       , htcurRow
                                       , gap_open_cost
                                       );
        /** Invariants block */
        tmp        = curRow;
        tmp1       = dncurRow;
        curRow     = prevRow;
        dncurRow   = pdncurRow;
        prevRow    = tmp;
        pdncurRow  = tmp1;
        dirMtx    += char2_len;
        curRow[0]  = prevRow[0];
        i++;
        len++;
    }

    return curRow;
}

static inline cost_t *
algn_fill_extending_left_right_affine ( const dyn_character_t    *char1
                                      ,       cost_t       *algn_precalcMtx
                                     // , size_t char1_len
                                      ,       size_t              char2_len
                                      ,       cost_t       *curRow
                                      ,       cost_t       *prevRow
                                      ,       DIR_MTX_ARROW_t    *dirMtx
                                      , const cost_matrices_2d_t *costMatrix
                                      ,       size_t              start_row
                                      ,       size_t              end_row
                                      ,       size_t              start_column
                                      ,       size_t              len
                                      ,       cost_t        *dncurRow
                                      ,       cost_t        *pdncurRow
                                      ,       cost_t        *htcurRow
                                      ,       cost_t         gap_open_cost
                                      )
{
    // printf("algn_fill_extending_left_right_affine\n");
    size_t i;

    cost_t *tmp,
                 *tmp1,
                  const_val,
                  prev_char1,
                  prev_const_val;

    elem_t cur_char1;

    const cost_t *gap_row,
                       *align_row;

    /** Invariants block
     * len is the number of cells to fill in the current row minus 1
     * start_column is the first cell to fill in the row */
    i = start_row;

    /* Conceptually,
       gap_row = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
    */
    gap_row = algn_precalcMtx + (costMatrix->gap_char * char2_len);
    len--;

    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_char1     = char1->char_begin[i - 1];
        cur_char1      = char1->char_begin[i];
        const_val      = cm_calc_cost_2d (costMatrix->cost, cur_char1,  costMatrix->gap_char, costMatrix->alphSize);
        prev_const_val = cm_calc_cost_2d (costMatrix->cost, prev_char1, costMatrix->gap_char, costMatrix->alphSize);
        /* Conceptually,
         * align_row = algnMtx_get_precal_row (algn_precalcMtx, cur_char1, char2_len);
         */
        align_row = algn_precalcMtx + (cur_char1 * char2_len);
        /* Align! */
        algn_fill_ukk_left_cell_affine ( curRow
                                       , prevRow
                                       , gap_row
                                       , align_row
                                       , dirMtx
                                       , const_val
                                       , prev_const_val
                                       , start_column
                                       , dncurRow
                                       , pdncurRow
                                       , htcurRow
                                       , gap_open_cost
                                       );

        algn_fill_row_affine ( curRow
                             , prevRow
                             , gap_row
                             , align_row
                             , dirMtx, const_val
                             , prev_const_val
                             , start_column + 1
                             , start_column + (len - 2)
                             , dncurRow
                             , pdncurRow
                             , htcurRow
                             , gap_open_cost
                             );

        algn_fill_ukk_right_cell_affine ( curRow
                                        , prevRow
                                        , gap_row
                                        , align_row
                                        , dirMtx
                                        , const_val
                                        , prev_const_val
                                        , start_column + len - 1
                                        , dncurRow
                                        , htcurRow
                                        , gap_open_cost
                                        );
        /** Invariants block */
        tmp       = curRow;
        tmp1      = dncurRow;
        curRow    = prevRow;
        dncurRow  = pdncurRow;
        prevRow   = tmp;
        pdncurRow = tmp1;
        i++;
        dirMtx   += char2_len;
        start_column++;
    }
    return (curRow);
}

cost_t *
algn_fill_extending_left_affine( const dyn_character_t    *char1
                               ,       cost_t       *algn_precalcMtx
                             // ,       size_t              char1_len
                               ,       size_t              char2_len
                               ,       cost_t       *curRow
                               ,       cost_t       *prevRow
                               ,       DIR_MTX_ARROW_t    *dirMtx
                               , const cost_matrices_2d_t *costMatrix
                               ,       size_t              start_row
                               ,       size_t              end_row
                               ,       size_t              start_column
                               ,       size_t              len
                               ,       cost_t       *dncurRow
                               ,       cost_t       *pdncurRow
                               ,       cost_t       *htcurRow
                               ,       cost_t        gap_open_cost
                               )
{
    // printf("algn_fill_extending_left_affine\n");
    cost_t *tmp,
                 *tmp1,
                  const_val,
                  prev_const_val,
                  const_val_tail,
                  const_val_tail_prev,
                 *gap_row,
                 *align_row;

     elem_t cur_char1,
            prev_char1;

    /** Invariants block
     *  start_column is the first cell to fill in the row
     *  len is the number of cells to fill in the current row minus 1
     */
    size_t i = start_row;

    /* Conceptually,
     * gap_row = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
     */
    gap_row = algn_precalcMtx + (costMatrix->gap_char * char2_len);

    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_char1          = char1->char_begin[i - 1];
        cur_char1           = char1->char_begin[i];
        prev_const_val      = cm_calc_cost_2d ( costMatrix->cost
                                              , prev_char1
                                              , costMatrix->gap_char
                                              , costMatrix->alphSize
                                              );
        const_val           = cm_calc_cost_2d ( costMatrix->cost
                                              , cur_char1
                                              , costMatrix->gap_char
                                              , costMatrix->alphSize
                                              );

        const_val_tail      = costMatrix->tail_cost[cur_char1];
        const_val_tail_prev = costMatrix->tail_cost[prev_char1];
        /* Conceptually,
         * align_row = algnMtx_get_precal_row (algn_precalcMtx, cur_char1, char2_len);
         */
        align_row = algn_precalcMtx + (cur_char1 * char2_len);
        /* Align! */
        algn_fill_ukk_left_cell_affine ( curRow
                                       , prevRow
                                       , gap_row
                                       , align_row
                                       , dirMtx
                                       , const_val
                                       , prev_const_val
                                       , start_column
                                       , dncurRow
                                       , pdncurRow
                                       , htcurRow
                                       , gap_open_cost);

        algn_fill_row_affine ( curRow
                             , prevRow
                             , gap_row
                             , align_row
                             , dirMtx
                             , const_val
                             , prev_const_val
                             , start_column + 1
                             , start_column + len - 1
                             , dncurRow
                             , pdncurRow
                             , htcurRow
                             , gap_open_cost);

        algn_fill_last_column_affine ( curRow
                                     , prevRow
                                     , const_val_tail
                                     , const_val_tail_prev
                                     , start_column + len - 1
                                     , dirMtx
                                     , dncurRow
                                     , pdncurRow
                                     , gap_open_cost);
        /** Invariants block */
        tmp       = curRow;
        tmp1      = dncurRow;
        curRow    = prevRow;
        dncurRow  = pdncurRow;
        prevRow   = tmp;
        pdncurRow = tmp1;
        dirMtx   += char2_len;
        i++;
        start_column++;
        len--;
    }
    if (DEBUG_COST_M) {
        printf ("A_A_A gap cost\n");
        fflush (stdout);
        for (i = 0; i < char2_len; i++) {
            printf ("%" cost_p "\t", gap_row[i]);
            fflush (stdout);
        }
        fflush (stdout);
    }

    return (curRow);
}

cost_t *
algn_fill_no_extending_affine ( const dyn_character_t    *char1
                              ,       cost_t       *algn_precalcMtx
                             // ,       size_t char1_len
                              ,       size_t              char2_len
                              ,       cost_t       *curRow
                              ,       cost_t       *prevRow
                              ,       DIR_MTX_ARROW_t    *dirMtx
                              , const cost_matrices_2d_t *costMatrix
                              ,       size_t              start_row
                              ,       size_t              end_row
                              ,       cost_t       *dncurRow
                              ,       cost_t       *pdncurRow
                              ,       cost_t       *htcurRow
                              ,       cost_t        gap_open_cost
                              )
{
    // printf("algn_fill_no_extending_affine\n");

    cost_t *tmp,
                  cur_char1,
                  const_val,
                  const_val_tail,
                  prev_char1,
                  prev_const_val,
                  const_val_tail_prev,
                 *tmp1;

    const cost_t *gap_row,
                       *align_row;

    /** Invariants block */
    size_t i = start_row;
    /* Conceptually,
     * gap_row = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
     */
    gap_row = algn_precalcMtx + (costMatrix->gap_char * char2_len);

    while (i < end_row) {
        /** Invariants block */
        assert (i > 0);
        prev_char1           = char1->char_begin[i - 1];
        cur_char1            = char1->char_begin[i];
        const_val           = cm_calc_cost_2d (costMatrix->cost, cur_char1,  costMatrix->gap_char, costMatrix->alphSize);
        prev_const_val      = cm_calc_cost_2d (costMatrix->cost, prev_char1, costMatrix->gap_char, costMatrix->alphSize);
        const_val_tail      = costMatrix->tail_cost[cur_char1];
        const_val_tail_prev = costMatrix->tail_cost[prev_char1];
        /* Conceptually,
         * align_row = algnMtx_get_precal_row (algn_precalcMtx, cur_char1, char2_len);
         */
        align_row           = algn_precalcMtx + (cur_char1 * char2_len);
        /* Align! */
        algn_fill_first_cell_affine ( curRow
                                   // , prevRow[0]
                                    , dirMtx
                                    , gap_open_cost
                                    , dncurRow
                                    , pdncurRow
                                    , htcurRow
                                    );

        algn_fill_row_affine ( curRow
                             , prevRow
                             , gap_row
                             , align_row
                             , dirMtx
                             , const_val
                             , prev_const_val
                             , 1
                             , char2_len - 1
                             , dncurRow
                             , pdncurRow
                             , htcurRow
                             , gap_open_cost);

        algn_fill_last_column_affine ( curRow
                                     , prevRow
                                     , const_val_tail
                                     , const_val_tail_prev
                                     , char2_len - 1
                                     , dirMtx
                                     , dncurRow
                                     , pdncurRow
                                     , gap_open_cost);
        /** Invariants block */
        tmp       = curRow;
        tmp1      = dncurRow;
        curRow    = prevRow;
        dncurRow  = pdncurRow;
        prevRow   = tmp;
        pdncurRow = tmp1;
        i++;
        dirMtx   += char2_len;
    }
    return (curRow);
}

/* SicurRowilar to the previous but when no barriers are set */
static inline cost_t
algn_fill_plane_affine ( const dyn_character_t    *char1
                       ,       cost_t       *algn_precalcMtx
                       ,       size_t              char1_len
                       ,       size_t              char2_len
                       ,       cost_t       *curRow
                       ,       DIR_MTX_ARROW_t    *dirMtx
                       , const cost_matrices_2d_t *costMatrix
                       ,       cost_t       *dncurRow
                       ,       cost_t       *htcurRow
                       ,       cost_t        gap_open_cost
                       )
{
    // printf("algn_fill_plane_affine\n");

    const cost_t *align_row,
                       *gap_row,
                       *first_gap_row;

    cost_t  const_val,
                  const_val_tail,
                  prev_const_val,
                  const_val_tail_prev,
                 *newNWMtx,
                 *tmp,
                 *tmp1,
                 *pdncurRow;

    size_t i;

    elem_t gap_char;

    /* A precalculated cost of a gap aligned with each base in the array */
    gap_char      = costMatrix->gap_char;
    gap_row       = algnMtx_get_precal_row (algn_precalcMtx, gap_char, char2_len);
    first_gap_row = algnMtx_get_precal_row (algn_precalcMtx, 0, char2_len);
    newNWMtx      = curRow;
    pdncurRow     = dncurRow;
    curRow[0]     = gap_open_cost;
    dirMtx[0]     = ALIGN | ALIGN_H | ALIGN_V;
    htcurRow[0]   = VERY_LARGE_NUMBER;
    dncurRow[0]   = VERY_LARGE_NUMBER;

    if (DEBUG_COST_M) {
        printf ("%" cost_p "\t", curRow[0]);
    }
    if (DEBUG_DIR_M) {
        printf ("A\t");
    }

    /* We fill the first row to start with */
    for (i = 1; i < char2_len; i++) {
        dncurRow[i] = VERY_LARGE_NUMBER;
        curRow[i]   = curRow[i - 1] + first_gap_row[i];
        dirMtx[i]   = INSERT | INSERT_H;
        if (DEBUG_COST_M) {
            printf ("%" cost_p "\t", curRow[i]);
            fflush (stdout);
        }
        if (DEBUG_DIR_M) {
            printf ("I\t");
        }
    }

    curRow     += char2_len;
    curRow[0]   = newNWMtx[0];
    newNWMtx[0] = 0;
    dncurRow   += char2_len;

    if (DEBUG_DIR_M || DEBUG_COST_M) {
        printf ("\n");
        fflush (stdout);
    }

    /* Now we fill the rest of the matrix */
    for (i = 1, dirMtx += char2_len; i < char1_len; i++, dirMtx += char2_len) {

        const_val_tail_prev = (costMatrix->tail_cost)[char1->char_begin[i - 1]];
        prev_const_val      = cm_calc_cost_2d( costMatrix->cost
                                             , char1->char_begin[i - 1]
                                             , costMatrix->gap_char
                                             , costMatrix->alphSize
                                             );
        const_val_tail      = (costMatrix->tail_cost)[char1->char_begin[i]];

        const_val           = cm_calc_cost_2d( costMatrix->cost
                                             , char1->char_begin[i]
                                             , costMatrix->gap_char
                                             , costMatrix->alphSize
                                             );
        align_row           = algnMtx_get_precal_row ( algn_precalcMtx
                                                , char1->char_begin[i]
                                                , char2_len
                                                );
        algn_fill_full_row_affine ( curRow
                                  , newNWMtx
                                  , gap_row
                                  , align_row
                                  , dirMtx
                                  , const_val
                                  , prev_const_val
                                  , const_val_tail
                                  , const_val_tail_prev
                                  , char2_len
                                  , dncurRow
                                  , pdncurRow
                                  , htcurRow
                                  , gap_open_cost
                                  );
        if (DEBUG_COST_M) {
            printf ("\n");
            fflush (stdout);
        }
        /* We swap curRow and newNWMtx for the next round */
        tmp       = curRow;
        tmp1      = dncurRow;
        curRow    = newNWMtx;
        dncurRow  = pdncurRow;
        newNWMtx  = tmp;
        pdncurRow = tmp1;
        curRow[0] = newNWMtx[0];
    }
    return (newNWMtx[char2_len - 1]);
}

static inline void
algn_choose_affine_other( cost_t *next_row
                        , cost_t *curRow
                        , cost_t **next_dncurRow
                        , cost_t **next_pdncurRow
                        , cost_t *dncurRow
                        , cost_t *pdncurRow
                        )
{
    if (next_row == curRow) {
        *next_dncurRow  = dncurRow;
        *next_pdncurRow = pdncurRow;
    }
    else {
        *next_dncurRow  = pdncurRow;
        *next_pdncurRow = dncurRow;
    }
}


static inline cost_t
HAS_GAP_EXTENSION (       elem_t              base
                  , const cost_matrices_2d_t *costMatrix
                  )
{
    return (cm_calc_cost_2d( costMatrix->cost
                           , base
                           , costMatrix->gap_char
                           , costMatrix->alphSize
                           )
            );
}

static inline cost_t
HAS_GAP_OPENING ( elem_t       prev
                , elem_t       curr
                , elem_t       gap_char
                , cost_t gap_open_cost
                )
{
    if   ((!(gap_char & prev)) && (gap_char & curr)) return 0;
    else                                             return gap_open_cost;
}


static inline void
FILL_EXTEND_HORIZONTAL_NOBT (       cost_t  sj_horizontal_extension
                            ,       cost_t  sj_gap_extension
                            ,       cost_t  longerChar_gap_open_cost
                            ,       int           j
                            ,       cost_t *extend_horizontal
//                            , const       cost_matrices_2d_t *c
                            , const cost_t *close_block_diagonal
                            )
{
    int ext_cost, open_cost;
    ext_cost = extend_horizontal[j - 1] + sj_horizontal_extension;
    open_cost = close_block_diagonal[j - 1] +
                longerChar_gap_open_cost + sj_gap_extension;
    if (DEBUG_AFFINE) {
        printf("\nFILL_EXTEND_HORIZONTAL_NOBT\n");
        printf ("Ext cost: %d, Open cost: %d, Gap extension: %" cost_p ", gap opening: %" cost_p ", sj_horizontal_extension: %" cost_p "\n",
                ext_cost, open_cost, sj_gap_extension, longerChar_gap_open_cost, sj_horizontal_extension);
    }
    if (ext_cost < open_cost)
        extend_horizontal[j] = ext_cost;
    else
        extend_horizontal[j] = open_cost;
    if (DEBUG_AFFINE)
        printf ("The final cost is %" cost_p "\n", extend_horizontal[j]);
}

DIR_MTX_ARROW_t
FILL_EXTEND_HORIZONTAL (       cost_t     sj_horizontal_extension
                       ,       cost_t     sj_gap_extension
                       ,       cost_t     longerChar_gap_open_cost
                       ,       int              j
                       ,       cost_t    *extend_horizontal
                       // , const cost_matrices_2d_t *costMatrix
                       , const cost_t    *close_block_diagonal
                       ,       DIR_MTX_ARROW_t  direction_matrix
                       )
{
    int ext_cost,
        open_cost;
    ext_cost  = extend_horizontal [j - 1]
              + sj_horizontal_extension;

    open_cost = close_block_diagonal[j - 1]
              + longerChar_gap_open_cost
              + sj_gap_extension;

    if (DEBUG_AFFINE) {
        printf("\nFILL_EXTEND_HORIZONTAL\n");
        printf ("Ext cost: %d, Open cost: %d, Gap extension: %" cost_p ", gap opening: %" cost_p ", sj_horizontal_extension: %" cost_p "\n",
                ext_cost, open_cost, sj_gap_extension, longerChar_gap_open_cost, sj_horizontal_extension);
    }
    if (ext_cost < open_cost) {
        LOR_WITH_DIR_MTX_ARROW_t(BEGIN_HORIZONTAL, direction_matrix);
        extend_horizontal[j] = ext_cost;
    }
    else {
        LOR_WITH_DIR_MTX_ARROW_t(END_HORIZONTAL, direction_matrix);
        extend_horizontal[j] = open_cost;
    }

    if (DEBUG_AFFINE) printf ("The final cost is %" cost_p "\n", extend_horizontal[j]);

    return (direction_matrix);
}

void
FILL_EXTEND_VERTICAL_NOBT (       cost_t  si_vertical_extension
                          ,       cost_t  si_gap_extension
                          ,       cost_t  lesserChar_gap_open_cost
                          ,       int           j
                          ,       cost_t *extend_vertical
                          , const cost_t *prev_extend_vertical
//                          , const cost_matrices_2d_t *c
                          , const cost_t *prev_close_block_diagonal
                          )
{
    int ext_cost,
        open_cost;

    ext_cost  = prev_extend_vertical[j]
              + si_vertical_extension;

    open_cost = prev_close_block_diagonal[j]
              + lesserChar_gap_open_cost
              + si_gap_extension;

    if (ext_cost < open_cost) extend_vertical[j] = ext_cost;
    else                      extend_vertical[j] = open_cost;
}

DIR_MTX_ARROW_t
FILL_EXTEND_VERTICAL (       cost_t  si_vertical_extension
                     ,       cost_t  si_gap_extension
                     ,       cost_t  lesserChar_gap_open_cost
                     ,       cost_t  j
                     ,       cost_t *extend_vertical
                     , const cost_t *prev_extend_vertical
//                     , const cost_matrices_2d_t *costMatrix
                     , const cost_t *prev_close_block_diagonal
                     ,       DIR_MTX_ARROW_t direction_matrix
                     )
{
    int ext_cost,
        open_cost;

    ext_cost  = prev_extend_vertical[j] + si_vertical_extension;

    open_cost = prev_close_block_diagonal[j]
              + lesserChar_gap_open_cost
              + si_gap_extension;

    if (ext_cost < open_cost) {
        LOR_WITH_DIR_MTX_ARROW_t(BEGIN_VERTICAL, direction_matrix);
        extend_vertical[j] = ext_cost;
    }
    else {
        LOR_WITH_DIR_MTX_ARROW_t(END_VERTICAL, direction_matrix);
        extend_vertical[j] = open_cost;
    }
    return (direction_matrix);
}

void
FILL_EXTEND_BLOCK_DIAGONAL_NOBT (       elem_t              original_lesserChar
                                ,       elem_t              original_longerChar
                                ,       elem_t              si_prev_base
                                // ,       elem_t              sj_prev_base
                                , const cost_matrices_2d_t *costMatrix
                                ,       int                 j
                                ,       cost_t       *extend_block_diagonal
                                , const cost_t       *prev_extend_block_diagonal
                                , const cost_t       *prev_close_block_diagonal
                                )
{
    cost_t ext_cost,
                 open_cost,
                 diag,
                 open_diag;

    int flag,
        flag2;

    elem_t gap_char      = costMatrix->gap_char;

    cost_t gap_open_cost = costMatrix->gap_open_cost;


    flag      = ( (gap_char & original_lesserChar)      && (  gap_char & original_longerChar));
    flag2     = (!(gap_char & si_prev_base) && (!(gap_char & original_longerChar)));

    diag      = flag ? 0 : VERY_LARGE_NUMBER;
    open_diag = flag ? (flag2 ? 0 : (2 * gap_open_cost)) : VERY_LARGE_NUMBER;

    ext_cost  = prev_extend_block_diagonal[j - 1] + diag;
    open_cost = prev_close_block_diagonal [j - 1] + open_diag;

    if   (ext_cost < open_cost) extend_block_diagonal[j] = ext_cost;
    else                        extend_block_diagonal[j] = open_cost;
}

DIR_MTX_ARROW_t
FILL_EXTEND_BLOCK_DIAGONAL (       elem_t              original_lesserChar
                           ,       elem_t              original_longerChar
                           // ,       elem_t              si_prev_base
                           // ,       elem_t              sj_prev_base
                           , const cost_matrices_2d_t * costMatrix
                           ,       cost_t        j
                           ,       cost_t       *extend_block_diagonal
                           , const cost_t       *prev_extend_block_diagonal
                           , const cost_t       *prev_close_block_diagonal
                           ,       DIR_MTX_ARROW_t     direction_matrix
                           )
{
    int ext_cost,
        open_cost,
        diag;
//    int open_diag;

    diag = ( (    costMatrix->gap_char & original_lesserChar)
              && (costMatrix->gap_char & original_longerChar)) ? 0 : VERY_LARGE_NUMBER;

/*
    if ( !(gap_char & si_prev_base)
        && (!(gap_char & original_longerChar))
        &&   (gap_char & original_lesserChar)
        &&   (gap_char & original_longerChar)
       ) {
        open_diag = 0;

    } else if (  (gap_char & original_lesserChar)
              && (gap_char & original_longerChar) ) {
        open_diag = 2 * gap_open_cost;
    } else {
        open_diag = VERY_LARGE_NUMBER;
    }
*/

/* following logic is reproduced legibly above
    open_diag = (    !(gap_char & si_prev_base)
                 && (!(gap_char & original_longerChar))
                 &&   (gap_char & original_lesserChar)
                 &&   (gap_char & original_longerChar)
                ) ?
        0 : (    ((gap_char & original_lesserChar)
              && (gap_char & original_longerChar)) ? (2 * gap_open_cost) : VERY_LARGE_NUMBER );
*/
    ext_cost  = prev_extend_block_diagonal[j - 1] + diag;
    open_cost = prev_close_block_diagonal [j - 1] + diag;
    if (ext_cost < open_cost) {
        LOR_WITH_DIR_MTX_ARROW_t(BEGIN_BLOCK, direction_matrix);
        extend_block_diagonal[j] = ext_cost;
    }
    else {
        LOR_WITH_DIR_MTX_ARROW_t(END_BLOCK, direction_matrix);
        extend_block_diagonal[j] = open_cost;
    }
    return (direction_matrix);
}


void
FILL_CLOSE_BLOCK_DIAGONAL_NOBT(       elem_t        original_lesserChar
                              ,       elem_t        original_longerChar
                              ,       elem_t        lesserChar_no_gap
                              ,       elem_t        longerChar_no_gap
                              ,       cost_t  lesserChar_gap_open_cost
                              ,       cost_t  longerChar_gap_open_cost
                              ,       cost_t  j
                              , const cost_t *c
                              ,       cost_t *close_block_diagonal
                              , const cost_t *prev_close_block_diagonal
                              , const cost_t *prev_extend_vertical
                              , const cost_t *prev_extend_horizontal
                              , const cost_t *prev_extend_block_diagonal
                              )
{
    cost_t diag,
                 extra_gap_opening,
                 algn,
                 from_vertical,
                 from_horizontal,
                 from_diagonal;

    diag = c[longerChar_no_gap];

    // diag = cm_calc_cost_2d(c->cost, lesserChar_no_gap, longerChar_no_gap, c->alphSize);

    extra_gap_opening = (longerChar_gap_open_cost < lesserChar_gap_open_cost) ? lesserChar_gap_open_cost : longerChar_gap_open_cost;

    if (DEBUG_AFFINE) {
        //printf ("Between %d and %d: Diag : %d, Extra gap opening: %d\n", lesserChar_no_gap, longerChar_no_gap, diag, extra_gap_opening);
        //fflush (stdout);
    }

    algn = prev_close_block_diagonal[j - 1] + diag;

    if   (original_lesserChar == lesserChar_no_gap) from_vertical = prev_extend_vertical[j - 1] + diag;
    else                                            from_vertical = prev_extend_vertical[j - 1] + diag + longerChar_gap_open_cost;

    if   (original_longerChar == longerChar_no_gap) from_horizontal = prev_extend_horizontal[j - 1] + diag;
    else                                            from_horizontal = prev_extend_horizontal[j - 1] + diag + lesserChar_gap_open_cost;

    from_diagonal           = prev_extend_block_diagonal[j - 1] + diag + extra_gap_opening;
    close_block_diagonal[j] = algn;

    if (close_block_diagonal[j] > from_vertical)   close_block_diagonal[j] = from_vertical;
    if (close_block_diagonal[j] > from_horizontal) close_block_diagonal[j] = from_horizontal;
    if (close_block_diagonal[j] > from_diagonal)   close_block_diagonal[j] = from_diagonal;
}

DIR_MTX_ARROW_t
FILL_CLOSE_BLOCK_DIAGONAL(       elem_t           original_lesserChar
                         ,       elem_t           original_longerChar
                         ,       elem_t           lesserChar_no_gap
                         ,       elem_t           longerChar_no_gap
                         ,       cost_t     lesserChar_gap_open_cost
                         ,       cost_t     longerChar_gap_open_cost
                         ,       int              j
                         , const cost_t    *c
                         ,       cost_t    *close_block_diagonal
                         , const cost_t    *prev_close_block_diagonal
                         , const cost_t    *prev_extend_vertical
                         , const cost_t    *prev_extend_horizontal
                         , const cost_t    *prev_extend_block_diagonal
                         ,       DIR_MTX_ARROW_t  direction_matrix
                         )
{
    cost_t diag,
                 extra_gap_opening,
                 algn,
                 from_vertical,
                 from_horizontal,
                 from_diagonal;

    DIR_MTX_ARROW_t mask;

    diag = c[longerChar_no_gap];
    // cm_calc_cost_2d(c->cost, lesserChar_no_gap, longerChar_no_gap, c->alphSize);
    extra_gap_opening =
        (longerChar_gap_open_cost < lesserChar_gap_open_cost)?lesserChar_gap_open_cost:longerChar_gap_open_cost;
    if (DEBUG_AFFINE) {
        // printf ("Between %d and %d: Diag : %d, Extra gap opening: %d\n", lesserChar_no_gap, longerChar_no_gap, diag, extra_gap_opening);
        // fflush (stdout);
    }
    algn = prev_close_block_diagonal[j - 1] + diag;
    if   (original_lesserChar == lesserChar_no_gap) from_vertical = prev_extend_vertical[j - 1] + diag;
    else                        from_vertical = prev_extend_vertical[j - 1] + diag + longerChar_gap_open_cost;

    if   (original_longerChar == longerChar_no_gap) from_horizontal = prev_extend_horizontal[j - 1] + diag;
    else                        from_horizontal = prev_extend_horizontal[j - 1] + diag + lesserChar_gap_open_cost;

    from_diagonal           = prev_extend_block_diagonal[j - 1] + diag + extra_gap_opening;
    mask                    = ALIGN_TO_ALIGN;
    close_block_diagonal[j] = algn;

    if (close_block_diagonal[j] >= from_vertical) {
        if (close_block_diagonal[j] > from_vertical) {
            close_block_diagonal[j] = from_vertical;
            mask = ALIGN_TO_VERTICAL;
        }
        else {
            mask = mask | ALIGN_TO_VERTICAL;
        }
    }

    if (close_block_diagonal[j] >= from_horizontal) {
        if (close_block_diagonal[j] > from_horizontal) {
            close_block_diagonal[j] = from_horizontal;
            mask = ALIGN_TO_HORIZONTAL;
        }
        else {
            mask = mask | ALIGN_TO_HORIZONTAL;
        }
    }

    if (close_block_diagonal[j] >= from_diagonal) {
        if (close_block_diagonal[j] > from_diagonal) {
            close_block_diagonal[j] = from_diagonal;
            mask = ALIGN_TO_DIAGONAL;
        }
        else {
            mask = mask | ALIGN_TO_DIAGONAL;
        }
    }

    LOR_WITH_DIR_MTX_ARROW_t(mask, direction_matrix);

    return (direction_matrix);
}

enum MODE { m_todo, m_vertical, m_horizontal, m_diagonal, m_align } backtrace_mode;


void
algn_backtrace_affine ( const dyn_character_t    *shortChar
                      , const dyn_character_t    *longerChar
                      ,       DIR_MTX_ARROW_t    *direction_matrix
                      ,       dyn_character_t    *gapped_median
                      ,       dyn_character_t    *ungapped_median
                      ,       dyn_character_t    *retShortChar
                      ,       dyn_character_t    *retLongChar
                      , const cost_matrices_2d_t *costMatrix
                      )
{
#define HAS_FLAG(flag) (*direction_matrix & flag)
    enum MODE mode = m_todo;
    int shortIdx,
        longIdx,
        shortChar_len,
        longerChar_len;

    elem_t shortCharElem,
           longerCharElem,
           prep;

    elem_t gap_char      = costMatrix->gap_char,
           all_ambiguous = gap_char - 1;

    DIR_MTX_ARROW_t *initial_direction_matrix;

    shortIdx      = shortChar->len - 1;
    longIdx       = longerChar->len - 1;
    shortChar_len = shortIdx;
    longerChar_len  = longIdx;

    assert (shortChar_len <= longerChar_len);

    shortCharElem            = shortChar->char_begin[shortIdx];
    longerCharElem             = longerChar->char_begin[longIdx];
    initial_direction_matrix = direction_matrix;
    direction_matrix         = direction_matrix + (((shortChar_len + 1) * (longerChar_len + 1)) - 1);

    while ((shortIdx != 0) && (longIdx != 0)) {
        if (DEBUG_AFFINE) {
            printf ("In position %d %d of affine backtrace\n", shortIdx, longIdx);
            fflush (stdout);
        }
        assert (initial_direction_matrix < direction_matrix);
        if (mode == m_todo) {
            if (HAS_FLAG(DO_HORIZONTAL)) {
                mode = m_horizontal;
            } else if (HAS_FLAG(DO_ALIGN)) {
                mode = m_align;
            } else if (HAS_FLAG(DO_VERTICAL)) {
                mode = m_vertical;
            } else {
                assert (HAS_FLAG(DO_DIAGONAL));
                mode = m_diagonal;
            }
        } else if (mode == m_vertical) {
            if (HAS_FLAG(END_VERTICAL)) {
                mode = m_todo;
            }
            if (!(shortCharElem & gap_char)) {
                dyn_char_prepend (gapped_median,   (shortCharElem | gap_char));
                dyn_char_prepend (ungapped_median, (shortCharElem | gap_char));
            } else {
                dyn_char_prepend (ungapped_median, gap_char);
            }
            dyn_char_prepend(retShortChar, shortCharElem);
            dyn_char_prepend(retLongChar, gap_char);
            shortIdx--;
            direction_matrix -= (longerChar_len + 1);
            shortCharElem = shortChar->char_begin[shortIdx];
        } else if (mode == m_horizontal) {
            if (HAS_FLAG(END_HORIZONTAL)) {
                mode = m_todo;
            }
            if (!(longerCharElem & gap_char)) {
                dyn_char_prepend (gapped_median,   (longerCharElem | gap_char));
                dyn_char_prepend (ungapped_median, (longerCharElem | gap_char));
            } else {
                dyn_char_prepend (ungapped_median, gap_char);
            }
            dyn_char_prepend (retShortChar, gap_char);
            dyn_char_prepend (retLongChar,  longerCharElem);
            longIdx--;
            direction_matrix -= 1;
            longerCharElem = longerChar->char_begin[longIdx];
        } else if (mode == m_diagonal) {
            if (HAS_FLAG(END_BLOCK)) {
                mode = m_todo;
            }
            dyn_char_prepend(retShortChar,    shortCharElem);
            dyn_char_prepend(retLongChar,     longerCharElem);
            dyn_char_prepend(ungapped_median, gap_char);
            shortIdx--;
            longIdx--;
            direction_matrix -= (longerChar_len + 2);
            longerCharElem       = longerChar->char_begin[longIdx];
            shortCharElem      = shortChar->char_begin[shortIdx];
        } else {
            assert (mode == m_align);
            if (HAS_FLAG(ALIGN_TO_HORIZONTAL)) {
                mode = m_horizontal;
            } else if (HAS_FLAG(ALIGN_TO_DIAGONAL)) {
                mode = m_diagonal;
            } else if (HAS_FLAG(ALIGN_TO_VERTICAL)) {
                mode = m_vertical;
            }
            prep = cm_get_median_2d( costMatrix
                                   , (shortCharElem & all_ambiguous)
                                   , (longerCharElem  & all_ambiguous)
                                   );

            dyn_char_prepend(gapped_median,   prep);
            dyn_char_prepend(ungapped_median, prep);
            dyn_char_prepend(retShortChar,    shortCharElem);
            dyn_char_prepend(retLongChar,     longerCharElem);

            shortIdx--;
            longIdx--;

            direction_matrix -= (longerChar_len + 2);
            longerCharElem    = longerChar->char_begin[longIdx];
            shortCharElem     = shortChar->char_begin[shortIdx];
        }
    }
    while (shortIdx != 0) {
        assert (initial_direction_matrix < direction_matrix);
        if (!(shortCharElem & gap_char)) {
            dyn_char_prepend (gapped_median,   (shortCharElem | gap_char));
            dyn_char_prepend (ungapped_median, (shortCharElem | gap_char));
        }
        else {
            dyn_char_prepend (ungapped_median, gap_char);
        }
        dyn_char_prepend(retShortChar, shortCharElem);
        dyn_char_prepend(retLongChar,  gap_char);
        direction_matrix -= (longerChar_len + 1);
        shortIdx--;
        shortCharElem = shortChar->char_begin[shortIdx];
    }
    while (longIdx != 0) {
        assert (initial_direction_matrix < direction_matrix);
        if (!(longerCharElem & gap_char)) {
            dyn_char_prepend (gapped_median,   (longerCharElem | gap_char));
            dyn_char_prepend (ungapped_median, (longerCharElem | gap_char));
        }
        else {
            dyn_char_prepend (ungapped_median, gap_char);
        }

        dyn_char_prepend (retShortChar, gap_char);
        dyn_char_prepend (retLongChar,  longerCharElem);
        longIdx--;
        direction_matrix -= 1;
        longerCharElem = longerChar->char_begin[longIdx];
    }
    dyn_char_prepend(retShortChar,    gap_char);
    dyn_char_prepend(retLongChar,     gap_char);
    dyn_char_prepend(ungapped_median, gap_char);

    if (gap_char != gapped_median->char_begin[0]) {
        dyn_char_prepend(gapped_median, gap_char);
    }
#undef HAS_FLAG

}

void
print_array ( char *title
            , const cost_t *arr
            , size_t max
            )
{
    printf ("%s", title);
    for (size_t i = 0; i <= max; i++) {
        printf ("%" cost_p " ", arr[i]);
    }
    printf ("\n");
    fflush (stdout);
}


void
print_dirMtx ( const char            *title
             , const DIR_MTX_ARROW_t *arr
             ,       size_t           max
             )
{
    printf ("%s", title);
    for (size_t i = 0; i <= max; i++) {
        printf ("%u ", arr[i]);
    }
    printf ("\n");
    fflush (stdout);
}


// nobt: no backtrace
void
algn_initialize_matrices_affine_nobt (      cost_t        gap_open_cost,
                                      const dyn_character_t    *si,
                                      const dyn_character_t    *sj,
                                      const cost_matrices_2d_t *c,
                                            cost_t       *close_block_diagonal,
                                            cost_t       *extend_block_diagonal,
                                            cost_t       *extend_vertical,
                                            cost_t       *extend_horizontal,
                                            cost_t       *algn_precalcMtx) {
    size_t longerChar_len,
           r,
           i = 1;
           // shortChar_len;

    cost_t *prev_extend_vertical,
                 *gap_row;

    elem_t  shortCharElem;
/*            longerCharElem,
         longerCharPrevElem,
         shortCharPrevElem;
*/

//    shortChar_len              = si->len - 1;
    longerChar_len            = sj->len - 1;
    close_block_diagonal[0]  = 0;
    extend_block_diagonal[0] = 0;
    extend_horizontal[0]     = gap_open_cost;
    extend_vertical[0]       = gap_open_cost;
    gap_row                  = algnMtx_get_precal_row(algn_precalcMtx, 0, longerChar_len);

    if (DEBUG_AFFINE) {
        printf("initialize_matrices_affine_nobt\n");
        printf ("\n\nThe gap opening parameter is %" cost_p "\n", gap_open_cost);
        printf ("\nPre-initialized values:\n");
        print_array ("EH: ", extend_horizontal,     longerChar_len);
        print_array ("EV: ", extend_vertical,       longerChar_len);
        print_array ("EB: ", extend_block_diagonal, longerChar_len);
        print_array ("CB: ", close_block_diagonal,  longerChar_len);
    }
    for (size_t j = 1; j <= longerChar_len; j++) {
//        longerCharElem              = sj->char_begin[j];
//        longerCharPrevElem          = sj->char_begin[j - 1];
        r                        = extend_horizontal[j - 1] + gap_row[j];
        extend_horizontal[j]     = r;
        close_block_diagonal[j]  = r;
        extend_block_diagonal[j] = VERY_LARGE_NUMBER;
        extend_vertical[j]       = VERY_LARGE_NUMBER;
    }
    if (DEBUG_AFFINE) {
        printf("initialize_matrices_affine_nobt\n");
        printf ("\nInitialized values:\n");
        print_array ("EH: ", extend_horizontal,     longerChar_len);
        print_array ("EV: ", extend_vertical,       longerChar_len);
        print_array ("EB: ", extend_block_diagonal, longerChar_len);
        print_array ("CB: ", close_block_diagonal,  longerChar_len);
        printf ("Finished initialization\n\n");
    }
    /* for (size_t i = 1; i <= shortChar_len; i++) { */
        prev_extend_vertical     = extend_vertical;
        extend_vertical         += (1 + longerChar_len);
        close_block_diagonal    += (1 + longerChar_len);
        extend_block_diagonal   += (1 + longerChar_len);
        extend_horizontal       += (1 + longerChar_len);
        shortCharElem            = si->char_begin[i];
//        shortCharPrevElem         = si->char_begin[i - 1];
        r                        = prev_extend_vertical[0] + (HAS_GAP_EXTENSION(shortCharElem, c));
        extend_horizontal[0]     = VERY_LARGE_NUMBER;
        close_block_diagonal[0]  = r;
        extend_block_diagonal[0] = VERY_LARGE_NUMBER;
        extend_vertical[0]       = r;
    /* } */
}


void
algn_initialize_matrices_affine(       cost_t        gap_open_cost
                               , const dyn_character_t    *shortChar
                               , const dyn_character_t    *longerChar
                               , const cost_matrices_2d_t *costMatrix
                               ,       cost_t       *close_block_diagonal
                               ,       cost_t       *extend_block_diagonal
                               ,       cost_t       *extend_vertical
                               ,       cost_t       *extend_horizontal
                               ,       cost_t       *final_cost_matrix
                               ,       DIR_MTX_ARROW_t    *direction_matrix
                               ,       cost_t       *algn_precalcMtx
                               )
{
    if (DEBUG_AFFINE) {
        printf("\ninitialize_matrices_affine\n");
        fflush(stdout);
    }
    cost_t longerChar_len,
                 r,
                *prev_extend_vertical;
                // shortChar_len;

    size_t i = 1;

    const cost_t *gap_row;
    elem_t /* longerCharElem, longerCharPrevElem, shortCharPrevElem, */ shortCharElem;
//    shortChar_len    = shortChar->len - 1; // This seems to be for deleting opening gap? This is currently unused

    longerChar_len           = longerChar->len - 1; // This also seems to be for deleting opening gap?
    final_cost_matrix[0]     = 0;
    close_block_diagonal[0]  = 0;
    extend_block_diagonal[0] = 0;
    extend_horizontal[0]     = gap_open_cost;
    extend_vertical[0]       = gap_open_cost;
    direction_matrix[0]      = 0xFFFF;
    gap_row                  = algnMtx_get_precal_row(algn_precalcMtx, 0, longerChar_len);

    if (DEBUG_AFFINE) {
        printf ("\n\nThe gap opening parameter is %" cost_p "\n", gap_open_cost);
        printf ("\nPre-initialized values:\n");
        print_array ("EH: ", extend_horizontal,     longerChar_len);
        print_array ("EV: ", extend_vertical,       longerChar_len);
        print_array ("EB: ", extend_block_diagonal, longerChar_len);
        print_array ("CB: ", close_block_diagonal,  longerChar_len);
        print_array ("FC: ", final_cost_matrix,     longerChar_len);
        print_dirMtx ("DM: ", direction_matrix,     longerChar_len);
    }

    for (size_t j = 1; j <= longerChar_len; j++) {
//        longerCharElem              = longerChar->char_begin[j];
//        longerCharPrevElem          = longerChar->char_begin[j - 1];
        r                        = extend_horizontal[j - 1] + gap_row[j];

        extend_horizontal[j]     = r;
        close_block_diagonal[j]  = r;
        final_cost_matrix[j]     = r;
        extend_block_diagonal[j] = VERY_LARGE_NUMBER;
        extend_vertical[j]       = VERY_LARGE_NUMBER;
        direction_matrix[j]      = DO_HORIZONTAL | END_HORIZONTAL;
    }

    if (DEBUG_AFFINE) {
        printf ("\nInitialized values:\n");
        print_array  ("EH: ", extend_horizontal,     longerChar_len);
        print_array  ("EV: ", extend_vertical,       longerChar_len);
        print_array  ("EB: ", extend_block_diagonal, longerChar_len);
        print_array  ("CB: ", close_block_diagonal,  longerChar_len);
        print_array  ("FC: ", final_cost_matrix,     longerChar_len);
        print_dirMtx ("DM: ", direction_matrix,      longerChar_len);
        printf ("Finished initializing.\n");
    }
    /* for (; i <= shortChar_len; i++) { */
        prev_extend_vertical   = extend_vertical;
        extend_vertical       += (1 + longerChar_len);
        close_block_diagonal  += (1 + longerChar_len);
        final_cost_matrix     += (1 + longerChar_len);
        extend_block_diagonal += (1 + longerChar_len);
        extend_horizontal     += (1 + longerChar_len);
        direction_matrix      += (1 + longerChar_len);

        shortCharElem          = shortChar->char_begin[i];
        // shortCharPrevElem       = shortChar->char_begin[i - 1];
        r                      = prev_extend_vertical[0] + (HAS_GAP_EXTENSION(shortCharElem, costMatrix));

        extend_horizontal[0]     = VERY_LARGE_NUMBER;
        close_block_diagonal[0]  = r;
        final_cost_matrix[0]     = r;
        extend_block_diagonal[0] = VERY_LARGE_NUMBER;
        extend_vertical[0]       = r;
        direction_matrix[0]      = DO_VERTICAL | END_VERTICAL;
    /* } */
}

DIR_MTX_ARROW_t
ASSIGN_MINIMUM (cost_t    *final_cost_matrix,
                cost_t     extend_horizontal,
                cost_t     extend_vertical,
                cost_t     extend_block_diagonal,
                cost_t     close_block_diagonal,
                DIR_MTX_ARROW_t  direction_matrix) {
    int mask;
    mask                        = DO_HORIZONTAL;
    *final_cost_matrix          = extend_horizontal;
    if (*final_cost_matrix     >= extend_vertical) {
        if (*final_cost_matrix > extend_vertical) {
            *final_cost_matrix = extend_vertical;
            mask               = DO_VERTICAL;
        } else {
            mask = mask | DO_VERTICAL;
        }
    }

    if (*final_cost_matrix >= extend_block_diagonal) {
        if (*final_cost_matrix > extend_block_diagonal) {
            *final_cost_matrix = extend_block_diagonal;
            mask               = DO_DIAGONAL;
        } else {
            mask = mask | DO_DIAGONAL;
        }
    }

    if (*final_cost_matrix >= close_block_diagonal) {
        if (*final_cost_matrix > close_block_diagonal) {
            *final_cost_matrix = close_block_diagonal;
            mask = DO_ALIGN;
        } else {
            mask = mask | DO_ALIGN;
        }
    }

    LOR_WITH_DIR_MTX_ARROW_t(mask, direction_matrix);
    return (direction_matrix);
}

cost_t
algn_fill_plane_2d_affine_nobt ( const dyn_character_t    *shortChar
                               , const dyn_character_t    *longerChar
                               ,       size_t              shortChar_len
                               ,       size_t              longerChar_len
                               ,       cost_matrices_2d_t *costMatrix
                               ,       cost_t       *extend_horizontal
                               ,       cost_t       *extend_vertical
                               ,       cost_t       *close_block_diagonal
                               ,       cost_t       *extend_block_diagonal
                               ,       cost_t       *algn_precalcMtx
                               ,       cost_t       *gap_open_prec
                               ,       cost_t       *longerChar_horizontal_extension
                               )
{
    size_t start_pos = 1,
           end_pos,
           start_v   = 40,
           j,
           res;

    cost_t *prev_extend_horizontal,
                 *prev_extend_vertical,
                 *prev_close_block_diagonal,
                 *prev_extend_block_diagonal;

    cost_t *init_extend_horizontal,
                 *init_extend_vertical,
                 *init_close_block_diagonal,
                 *init_extend_block_diagonal;

    cost_t *shortChar_no_gap_vector;

    cost_t  lesserChar_gap_open_cost,
                  lesserChar_gap_extend_cost,
                  longerChar_gap_open_cost,
                  longerChar_gap_extension,
                  shortChar_vertical_extension,
                  gap_open_cost                 = costMatrix->gap_open_cost;

    elem_t gap_char      = costMatrix->gap_char,
           all_ambiguous = costMatrix->gap_char - 1;


    const cost_t *gap_row;

    assert (longerChar_len >= shortChar_len);

    init_extend_horizontal     = extend_horizontal;
    init_extend_vertical       = extend_vertical;
    init_extend_block_diagonal = extend_block_diagonal;
    init_close_block_diagonal  = close_block_diagonal;

    gap_row = algnMtx_get_precal_row(algn_precalcMtx, 0, longerChar_len);
    end_pos = (longerChar_len - shortChar_len) + 8;

    if (DEBUG_AFFINE) {
        printf("\n--algn fill plane 3 affine nobt\n");
        printf("Before initializing:\n");
        print_array ("EH: ", extend_horizontal,     longerChar_len);
        print_array ("EV: ", extend_vertical,       longerChar_len);
        print_array ("EB: ", extend_block_diagonal, longerChar_len);
        print_array ("CB: ", close_block_diagonal,  longerChar_len);
    }

    if (end_pos < 40) end_pos = 40;

    if (end_pos > longerChar_len) end_pos = longerChar_len;
    elem_t  longerCharElem,
           // longerCharPrevElem,
            shortCharElem,
            shortCharPrevElem,
            shortChar_no_gap,
            longerChar_no_gap;

    elem_t *shortCharBegin,
           *longerCharBegin;

    shortCharBegin = shortChar->char_begin;
    longerCharBegin  = longerChar->char_begin;
    shortCharElem  = shortCharBegin[0];

    for (j = 1; j <= longerChar_len; j++) {
        gap_open_prec[j] = HAS_GAP_OPENING(longerCharBegin[j - 1], longerCharBegin[j], gap_char, gap_open_cost);

        if ((longerCharBegin[j - 1] & gap_char) && (!(longerCharBegin[j] & gap_char))) {
            longerChar_horizontal_extension[j] = gap_open_prec[j] + gap_row[j];
        } else {
            longerChar_horizontal_extension[j] = gap_row[j];
        }
    }

    longerChar_horizontal_extension[1] = gap_row[1];
    int r;

    for (size_t i = 1; i <= shortChar_len; i++) {
        prev_extend_horizontal = init_extend_horizontal
                               + ((i - 1) % 2) * (longerChar_len + 1);

        prev_extend_vertical = init_extend_vertical
                             + (longerChar_len + 1) * ((i - 1) % 2);

        prev_extend_block_diagonal = init_extend_block_diagonal
                                   + (longerChar_len + 1) * ((i - 1) % 2);

        prev_close_block_diagonal = init_close_block_diagonal
                                  + (longerChar_len + 1) * ((i - 1) % 2);

        extend_horizontal = init_extend_horizontal + ((i % 2)           * (longerChar_len + 1));
        extend_vertical   = init_extend_vertical   + (longerChar_len + 1) * (i % 2);

        extend_block_diagonal = init_extend_block_diagonal
                              + (longerChar_len + 1) * (i % 2);

        close_block_diagonal = init_close_block_diagonal
                             + (longerChar_len + 1) * (i % 2);

        if (i > start_v) start_pos++;

        extend_horizontal[start_pos - 1] = VERY_LARGE_NUMBER;
        shortCharPrevElem          = shortCharElem;
        shortCharElem              = shortCharBegin[i];
        lesserChar_gap_extend_cost = HAS_GAP_EXTENSION(shortCharElem, costMatrix);
        lesserChar_gap_open_cost   = HAS_GAP_OPENING (shortCharPrevElem, shortCharElem, gap_char, gap_open_cost);
        shortChar_no_gap           = (all_ambiguous) & shortCharElem;

        if ( (i > 1)
             && (     (shortCharPrevElem & gap_char)
                  && !(shortCharElem     & gap_char)
                )
           ) {
            shortChar_vertical_extension = lesserChar_gap_open_cost + lesserChar_gap_extend_cost;
        } else {
            shortChar_vertical_extension = lesserChar_gap_extend_cost;
        }

        r = prev_extend_vertical[start_pos - 1] + shortChar_vertical_extension;

        extend_horizontal    [start_pos - 1] = VERY_LARGE_NUMBER;
        close_block_diagonal [start_pos - 1] = r;
        extend_block_diagonal[start_pos - 1] = VERY_LARGE_NUMBER;
        extend_vertical      [start_pos - 1] = r;
        longerCharElem                       = longerCharBegin[start_pos - 1];
        close_block_diagonal [start_pos - 1] = VERY_LARGE_NUMBER;

        shortChar_no_gap_vector = costMatrix->cost + (shortChar_no_gap << costMatrix->costMatrixDimension);

        for (j=start_pos; j <= end_pos; j++) {
//            longerCharPrevElem       = longerCharElem;
            longerCharElem           = longerCharBegin[j];
            longerChar_no_gap        = (all_ambiguous) & longerCharElem;
            longerChar_gap_extension = gap_row[j];
            longerChar_gap_open_cost   = gap_open_prec[j];
            FILL_EXTEND_HORIZONTAL_NOBT( longerChar_horizontal_extension[j]
                                       , longerChar_gap_extension
                                       , longerChar_gap_open_cost
                                       , j
                                       , extend_horizontal
//                                       , costMatrix
                                       , close_block_diagonal
                                       );

            FILL_EXTEND_VERTICAL_NOBT( shortChar_vertical_extension
                                     , lesserChar_gap_extend_cost
                                     , lesserChar_gap_open_cost
                                     , j
                                     , extend_vertical
                                     , prev_extend_vertical
//                                     , costMatrix
                                     , prev_close_block_diagonal
                                     );

            FILL_EXTEND_BLOCK_DIAGONAL_NOBT( shortCharElem
                                           , longerCharElem
                                           , shortCharPrevElem
//                                           , longerCharPrevElem
                                           , costMatrix
                                           , j
                                           , extend_block_diagonal
                                           , prev_extend_block_diagonal
                                           , prev_close_block_diagonal
                                           );

            FILL_CLOSE_BLOCK_DIAGONAL_NOBT( shortCharElem
                                          , longerCharElem
                                          , shortChar_no_gap
                                          , longerChar_no_gap
                                          , lesserChar_gap_open_cost
                                          , longerChar_gap_open_cost
                                          , j
                                          , shortChar_no_gap_vector
                                          , close_block_diagonal
                                          , prev_close_block_diagonal
                                          , prev_extend_vertical
                                          , prev_extend_horizontal
                                          , prev_extend_block_diagonal
                                          );
        }
        if (end_pos < longerChar_len) {
            end_pos++;
            extend_vertical[end_pos]       = VERY_LARGE_NUMBER;
            close_block_diagonal[end_pos]  = VERY_LARGE_NUMBER;
            extend_horizontal[end_pos]     = VERY_LARGE_NUMBER;
            extend_block_diagonal[end_pos] = VERY_LARGE_NUMBER;
        }
        if (DEBUG_AFFINE) {
            printf("algn fill plane 3 affine nobt\n");
            printf("After initializing:\n");
            print_array ("EH: ", extend_horizontal,     longerChar_len);
            print_array ("EV: ", extend_vertical,       longerChar_len);
            print_array ("EB: ", extend_block_diagonal, longerChar_len);
            print_array ("CB: ", close_block_diagonal,  longerChar_len);
        }
    }

    res = extend_horizontal[longerChar_len];
    if (res > extend_vertical[longerChar_len])       res = extend_vertical[longerChar_len];
    if (res > extend_block_diagonal[longerChar_len]) res = extend_block_diagonal[longerChar_len];
    if (res > close_block_diagonal[longerChar_len])  res = close_block_diagonal[longerChar_len];

    return res;
}


cost_t
algn_fill_plane_2d_affine ( const dyn_character_t    *shortChar
                          , const dyn_character_t    *longerChar
                          ,       size_t              shortChar_len    // note that this is actually 1 less than length
                          ,       size_t              longerChar_len   // note that this is actually 1 less than length
                          ,       cost_t       *final_cost_matrix
                          ,       DIR_MTX_ARROW_t    *direction_matrix
                          , const cost_matrices_2d_t * costMatrix
                          ,       cost_t       *extend_horizontal
                          ,       cost_t       *extend_vertical
                          ,       cost_t       *close_block_diagonal
                          ,       cost_t       *extend_block_diagonal
                          ,       cost_t       *algn_precalcMtx
                          ,       cost_t       *gap_open_prec
                          ,       cost_t       *longerChar_horizontal_extension
                          )
{
    if (DEBUG_AFFINE) {
        printf("algn_fill_plane_2d_affine\n");
        fflush(stdout);
    }
    size_t start_pos    = 1,
           end_pos,
           start_v      = 40,
           res,
           longerCharIdx;

    cost_t *prev_extend_horizontal,
                 *prev_extend_vertical,
                 *prev_close_block_diagonal,
                 *prev_extend_block_diagonal;

    cost_t *init_extend_horizontal,
                 *init_extend_vertical,
                 *init_close_block_diagonal,
                 *init_extend_block_diagonal;

    cost_t shortChar_vertical_extension;

    const cost_t *shortChar_no_gap_vector;

    cost_t lesserChar_gap_open_cost,
                 lesserChar_gap_extend_cost,
                 longerChar_gap_open_cost,
                 longerChar_gap_extension;

    elem_t gap_char      = costMatrix->gap_char,
           gap_open_cost = costMatrix->gap_open_cost,
           all_ambiguous = costMatrix->gap_char - 1;

    cost_t *gap_row;

    DIR_MTX_ARROW_t tmp_direction_matrix;

    init_extend_horizontal     = extend_horizontal;
    init_extend_vertical       = extend_vertical;
    init_extend_block_diagonal = extend_block_diagonal;
    init_close_block_diagonal  = close_block_diagonal;
    gap_row                    = algnMtx_get_precal_row(algn_precalcMtx, 0, longerChar_len);
    end_pos                    = (longerChar_len - shortChar_len) + 8;
    if (DEBUG_AFFINE) {
        printf("\n--algn fill plane 3 affine\n");
        printf("Before initializing:\n");
        print_array  ("EH: ", extend_horizontal,     longerChar_len);
        print_array  ("EV: ", extend_vertical,       longerChar_len);
        print_array  ("EB: ", extend_block_diagonal, longerChar_len);
        print_array  ("CB: ", close_block_diagonal,  longerChar_len);
        print_array  ("FC: ", final_cost_matrix,     longerChar_len);
        print_dirMtx ("DM: ", direction_matrix,      longerChar_len);
    }

    if (end_pos < 40) end_pos = 40;

    if (end_pos > longerChar_len) end_pos = longerChar_len;

    //end_pos = longerChar_len;
    elem_t  longerCharElem,
            // longerCharPrevElem,
            shortCharElem,
            shortCharPrevElem,
            shortChar_no_gap,
            longerChar_no_gap,
           *shortCharBegin,
           *longerCharBegin;

    shortCharBegin = shortChar->char_begin;
    longerCharBegin  = longerChar->char_begin;
    shortCharElem  = shortCharBegin[0];

    for (longerCharIdx = 1; longerCharIdx <= longerChar_len; longerCharIdx++) {
        // if (shortChar->len >= longerChar->len) {
        //     printf("short j: %2zu len: %2zu cap: %2zu\n", longerCharIdx, shortChar->len, shortChar->cap);
        //     printf("long  j: %2zu len: %2zu cap: %2zu\n", longerCharIdx, longerChar->len,  longerChar->cap);
        //     fflush(stdout);
        // }
        // printf("\tlongerCharBegin %d\n", longerCharBegin[j - 1]);
        // printf("\tlongerCharBegin %d\n", longerCharBegin[j]);
        // printf("\tlongerChar_horizontal_extension %d\n", longerChar_horizontal_extension[j - 1]);
        // printf("\tgap_open_prec %d\n", gap_open_prec[j]);
        // printf("\tgap_row %d\n", gap_row[j]);
        // printf("\tgap_char %d\n", gap_char);
        gap_open_prec[longerCharIdx] = HAS_GAP_OPENING(longerCharBegin[longerCharIdx - 1], longerCharBegin[longerCharIdx], gap_char, gap_open_cost);
        if (     (longerCharBegin[longerCharIdx - 1] & gap_char)
            && (!(longerCharBegin[longerCharIdx]     & gap_char)) ) {

            longerChar_horizontal_extension[longerCharIdx] = gap_open_prec[longerCharIdx] + gap_row[longerCharIdx];
        } else {
            longerChar_horizontal_extension[longerCharIdx] = gap_row[longerCharIdx];
        }
    }
    longerChar_horizontal_extension[1] = gap_row[1];
    int r;
    for (size_t shortCharIdx = 1; shortCharIdx <= shortChar_len; shortCharIdx++) {
        // printf("i: %zu\n", i);
        //printf("%d, %d\n", shortCharBegin[i - 1], shortCharBegin[i]);

        prev_extend_horizontal     = init_extend_horizontal +
                                       (((shortCharIdx - 1) % 2) * (longerChar_len + 1));
        prev_extend_vertical       = init_extend_vertical +
                                       ((longerChar_len + 1) * ((shortCharIdx - 1) % 2));
        prev_extend_block_diagonal = init_extend_block_diagonal + ((longerChar_len + 1) * ((shortCharIdx - 1) % 2));
        prev_close_block_diagonal  = init_close_block_diagonal +
                                       ((longerChar_len + 1) * ((shortCharIdx - 1) % 2));
        extend_horizontal          = init_extend_horizontal + ((shortCharIdx % 2) * (longerChar_len + 1));
        extend_vertical            = init_extend_vertical + ((longerChar_len + 1) * (shortCharIdx % 2));
        extend_block_diagonal      = init_extend_block_diagonal + ((longerChar_len + 1) * (shortCharIdx % 2));
        close_block_diagonal       = init_close_block_diagonal + ((longerChar_len + 1) * (shortCharIdx % 2));
        direction_matrix           = direction_matrix + (longerChar_len + 1);

        if (shortCharIdx > start_v) {
            start_pos++;
        }
        direction_matrix [start_pos - 1] = DO_VERTICAL | END_VERTICAL;
        extend_horizontal[start_pos - 1] = VERY_LARGE_NUMBER;
        shortCharPrevElem        = shortCharElem;
        shortCharElem            = shortCharBegin[shortCharIdx];
        lesserChar_gap_extend_cost  = HAS_GAP_EXTENSION(shortCharElem, costMatrix);
        lesserChar_gap_open_cost = HAS_GAP_OPENING (shortCharPrevElem, shortCharElem, gap_char, gap_open_cost);
        shortChar_no_gap         = (all_ambiguous) & shortCharElem;

        if ((shortCharIdx > 1) && ((shortCharPrevElem & gap_char) && (!(shortCharElem & gap_char)))) {
            shortChar_vertical_extension = lesserChar_gap_open_cost + lesserChar_gap_extend_cost;
        } else {
            shortChar_vertical_extension = lesserChar_gap_extend_cost;
        }
        r = prev_extend_vertical[start_pos - 1] + shortChar_vertical_extension;
        extend_horizontal       [start_pos - 1] = VERY_LARGE_NUMBER;
        close_block_diagonal    [start_pos - 1] = r;
        final_cost_matrix       [start_pos - 1] = r;
        extend_block_diagonal   [start_pos - 1] = VERY_LARGE_NUMBER;
        extend_vertical         [start_pos - 1] = r;
        direction_matrix        [start_pos - 1] = DO_VERTICAL | END_VERTICAL;
        longerCharElem                          = longerCharBegin[start_pos - 1];
        close_block_diagonal    [start_pos - 1] = VERY_LARGE_NUMBER;
        shortChar_no_gap_vector                 = costMatrix->cost + (shortChar_no_gap << costMatrix->costMatrixDimension);

        for (longerCharIdx = start_pos; longerCharIdx <= end_pos; longerCharIdx++) {
//            longerCharPrevElem       = longerCharElem;
            longerCharElem           = longerCharBegin[longerCharIdx];
            tmp_direction_matrix   = 0;
            longerChar_no_gap        = (all_ambiguous) & longerCharElem;
            longerChar_gap_extension = gap_row[longerCharIdx];
            longerChar_gap_open_cost   = gap_open_prec[longerCharIdx];

            tmp_direction_matrix = FILL_EXTEND_HORIZONTAL( longerChar_horizontal_extension[longerCharIdx]
                                                         , longerChar_gap_extension
                                                         , longerChar_gap_open_cost
                                                         , longerCharIdx
                                                         , extend_horizontal
//                                                         , c
                                                         , close_block_diagonal
                                                         , tmp_direction_matrix
                                                         );

            tmp_direction_matrix = FILL_EXTEND_VERTICAL( shortChar_vertical_extension
                                                       , lesserChar_gap_extend_cost
                                                       , lesserChar_gap_open_cost
                                                       , longerCharIdx
                                                       , extend_vertical
                                                       , prev_extend_vertical
//                                                       , c
                                                       , prev_close_block_diagonal
                                                       , tmp_direction_matrix
                                                       );

            tmp_direction_matrix = FILL_EXTEND_BLOCK_DIAGONAL( shortCharElem
                                                             , longerCharElem
//                                                             , shortCharPrevElem
//                                                             , longerCharPrevElem
                                                             , costMatrix
                                                             , longerCharIdx
                                                             , extend_block_diagonal
                                                             , prev_extend_block_diagonal
                                                             , prev_close_block_diagonal
                                                             , tmp_direction_matrix
                                                             );

            tmp_direction_matrix = FILL_CLOSE_BLOCK_DIAGONAL( shortCharElem
                                                            , longerCharElem
                                                            , shortChar_no_gap
                                                            , longerChar_no_gap
                                                            , lesserChar_gap_open_cost
                                                            , longerChar_gap_open_cost
                                                            , longerCharIdx
                                                            , shortChar_no_gap_vector
                                                            , close_block_diagonal
                                                            , prev_close_block_diagonal
                                                            , prev_extend_vertical
                                                            , prev_extend_horizontal
                                                            , prev_extend_block_diagonal
                                                            , tmp_direction_matrix
                                                            );

            tmp_direction_matrix = ASSIGN_MINIMUM ( final_cost_matrix + longerCharIdx
                                                  , extend_horizontal[longerCharIdx]
                                                  , extend_vertical[longerCharIdx]
                                                  , extend_block_diagonal[longerCharIdx]
                                                  , close_block_diagonal[longerCharIdx]
                                                  , tmp_direction_matrix
                                                  );

            direction_matrix[longerCharIdx]  = tmp_direction_matrix;
        }
        if (end_pos < longerChar_len) {
            end_pos++;
            direction_matrix[end_pos]      = DO_HORIZONTAL | END_HORIZONTAL;
            extend_vertical[end_pos]       = VERY_LARGE_NUMBER;
            close_block_diagonal[end_pos]  = VERY_LARGE_NUMBER;
            extend_horizontal[end_pos]     = VERY_LARGE_NUMBER;
            extend_horizontal[end_pos]     = VERY_LARGE_NUMBER;
            extend_block_diagonal[end_pos] = VERY_LARGE_NUMBER;
        }
        if (DEBUG_AFFINE) {
            printf("\n--algn fill plane 3 affine\n");
            printf("Inside loop:\n");
            print_array ("EH: ", extend_horizontal,     longerChar_len);
            print_array ("EV: ", extend_vertical,       longerChar_len);
            print_array ("EB: ", extend_block_diagonal, longerChar_len);
            print_array ("CB: ", close_block_diagonal,  longerChar_len);
            print_array ("FC: ", final_cost_matrix,     longerChar_len);
            print_dirMtx ("DM: ", direction_matrix,     longerChar_len);
        }
    }
    res = final_cost_matrix[longerChar_len];
    return res;
}


static inline int
algn_fill_plane_2_affine ( const dyn_character_t    *longerChar
                         ,       cost_t       *algn_precalcMtx
                         ,       size_t              longerChar_len
                         ,       size_t              lesserChar_len
                         ,       cost_t       *curRow
                         ,       DIR_MTX_ARROW_t    *dirMtx
                         , const cost_matrices_2d_t *costMatrix
                         ,       size_t              width
                         ,       size_t              height
                         ,       size_t              deltawh
                         ,       cost_t       *dncurRow
                         ,       cost_t       *htcurRow
                         )
{
    cost_t *next_row,
                 *next_prevRow,
                 *next_dncurRow,
                 *next_pdncurRow,
              // *a,
                 *b,
                 *d,
                 *e,
                  gap_open_cost,
                  start_row,
                  final_row,
                  start_column,
                  length,
                 *gap_row;

    DIR_MTX_ARROW_t *to_go_dirMtx;

    gap_open_cost = costMatrix->gap_open_cost;
    width    = width + deltawh;

    if (width > lesserChar_len) {
        width = lesserChar_len;
    }

    height = height + deltawh;
    if (height > longerChar_len) {
        height = longerChar_len;
    }
    // a = curRow;
    b = curRow + lesserChar_len;
    d = dncurRow;
    e = dncurRow + lesserChar_len;
    gap_row = algnMtx_get_precal_row (algn_precalcMtx, 0, lesserChar_len); /* We want the horizontal row */
    /* We have to consider three cases in this new alignment procedure (much
     * cleaner than the previous):
     *
     * Case 1:
     * If longerChar is much longer than char2, then there is no point on using the
     * barriers, we rather fill the full matrix in one shot */
    if ( longerChar_len > 1.5 * lesserChar_len ) {
        return (algn_fill_plane_affine ( longerChar
                                       , algn_precalcMtx
                                       , longerChar_len
                                       , lesserChar_len
                                       , curRow
                                       , dirMtx
                                       , costMatrix
                                       , d
                                       , htcurRow
                                       , gap_open_cost
                                       )
               );
    }
    /* Case 2:
     * There are no full rows to be filled, therefore we have to break the
     * procedure in three different subsets */
    else if ( 2 * height < longerChar_len) {
        algn_fill_first_row_affine ( curRow, dirMtx, width, gap_row, d, htcurRow, gap_open_cost);
        b[0] = curRow[0];
        curRow[0] = 0;
        start_row    = 1;
        final_row    = height;
        start_column = 0;
        length       = width + 1;
        to_go_dirMtx = dirMtx + (start_row * lesserChar_len);
        /* Now we fill that space */
        next_row     = algn_fill_extending_right_affine ( longerChar
                                                        , algn_precalcMtx
                                                       // , longerChar_len
                                                        , lesserChar_len
                                                        , b
                                                        , curRow
                                                        , to_go_dirMtx
                                                        , costMatrix
                                                        , start_row
                                                        , final_row
                                                        , length
                                                        , e
                                                        , d
                                                        , htcurRow
                                                        , gap_open_cost
                                                        );

        next_prevRow = choose_other (next_row, curRow, b);

        algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);

        /* Next group */
        start_row    = final_row;
        final_row    = longerChar_len - (height - 1);
        start_column = 1;
        length       = width + height;
        to_go_dirMtx = dirMtx + (start_row * lesserChar_len);

        next_row = algn_fill_extending_left_right_affine ( longerChar
                                                         , algn_precalcMtx
                                                        // , longerChar_len
                                                         , lesserChar_len
                                                         , next_row
                                                         , next_prevRow
                                                         , to_go_dirMtx
                                                         , costMatrix
                                                         , start_row
                                                         , final_row
                                                         , start_column
                                                         , length
                                                         , next_dncurRow
                                                         , next_pdncurRow
                                                         , htcurRow
                                                         , gap_open_cost
                                                         );

        next_prevRow = choose_other (next_row, curRow, b);

        algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);

        /* The final group */
        start_row    = final_row;
        final_row    = longerChar_len;
        length       = length - 2;
        start_column = lesserChar_len - length;
        to_go_dirMtx = dirMtx + (start_row * lesserChar_len);

        next_row     = algn_fill_extending_left_affine ( longerChar
                                                       , algn_precalcMtx
                                                      // , longerChar_len
                                                       , lesserChar_len
                                                       , next_row
                                                       , next_prevRow
                                                       , to_go_dirMtx
                                                       , costMatrix
                                                       , start_row
                                                       , final_row
                                                       , start_column
                                                       , length
                                                       , next_dncurRow
                                                       , next_pdncurRow
                                                       , htcurRow
                                                       , gap_open_cost
                                                       );

        next_prevRow = choose_other (next_row, curRow, b);

        algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);
    }
    /* Case 3: (final case)
     * There is a block in the middle of with full rows that have to be filled
     * */
    else {
        /* We will simplify this case even further, if the size of the leftover
         * is too small, don't use the barriers at all, just fill it up all */
        if (8 >= (longerChar_len - height)) {
            return ( algn_fill_plane_affine ( longerChar
                                            , algn_precalcMtx
                                            , longerChar_len
                                            , lesserChar_len
                                            , curRow
                                            , dirMtx
                                            , costMatrix
                                            , d
                                            , htcurRow
                                            , gap_open_cost
                                            )
                   );
        } else {
            algn_fill_first_row_affine ( curRow
                                       , dirMtx
                                       , width
                                       , gap_row
                                       , dncurRow
                                       , htcurRow
                                       , gap_open_cost
                                       );

            b[0]         = curRow[0];
            curRow[0]    = 0;
            start_row    = 1;
            final_row    = (lesserChar_len - width) + 1;
            start_column = 0;
            length       = width + 1;
            to_go_dirMtx = dirMtx + (lesserChar_len * start_row);
            next_row     = algn_fill_extending_right_affine ( longerChar
                                                            , algn_precalcMtx
                                                           // , longerChar_len
                                                            , lesserChar_len
                                                            , b
                                                            , curRow
                                                            , to_go_dirMtx
                                                            , costMatrix
                                                            , start_row
                                                            , final_row
                                                            , length
                                                            , e
                                                            , d
                                                            , htcurRow
                                                            , gap_open_cost
                                                            );
            next_prevRow = choose_other (next_row, curRow, b);

            algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);

            start_row    = final_row;
            final_row    = longerChar_len - (lesserChar_len - width) + 1;
            length       = lesserChar_len;
            to_go_dirMtx = dirMtx + (lesserChar_len * start_row);

            next_row     = algn_fill_no_extending_affine ( longerChar
                                                         , algn_precalcMtx
                                                        // , longerChar_len
                                                         , lesserChar_len
                                                         , next_row
                                                         , next_prevRow
                                                         , to_go_dirMtx
                                                         , costMatrix
                                                         , start_row
                                                         , final_row
                                                         , next_dncurRow
                                                         , next_pdncurRow
                                                         , htcurRow
                                                         , gap_open_cost
                                                         );
            next_prevRow = choose_other (next_row, curRow, b);

            algn_choose_affine_other (next_row, curRow, &next_dncurRow, &next_pdncurRow, d, e);

            start_row    = final_row;
            final_row    = longerChar_len;
            start_column = 1;
            length       = lesserChar_len - 1;
            to_go_dirMtx = dirMtx + (lesserChar_len * start_row);

            next_row     = algn_fill_extending_left_affine ( longerChar
                                                           , algn_precalcMtx
                                                          // , longerChar_len
                                                           , lesserChar_len
                                                           , next_row
                                                           , next_prevRow
                                                           , to_go_dirMtx
                                                           , costMatrix
                                                           , start_row
                                                           , final_row
                                                           , start_column
                                                           , length
                                                           , next_dncurRow
                                                           , next_pdncurRow
                                                           , htcurRow
                                                           , gap_open_cost
                                                           );
            next_prevRow = choose_other (next_row, curRow, b);
        }
    }
    return (next_prevRow[lesserChar_len - 1]);
}


/** [algn_nw_limit_2d performs a pairwise
 *  alignment of the subcharacters of char1 and char2 starting in position st_char1
 *  and st_char2, respectively, with length len_char1 and len_char2 using the
 *  transformation cost matrix cstMtx and the alignment matrices algnMats.
 */
static inline int
algn_nw_limit_2d ( const dyn_character_t      *shorterChar
                 , const dyn_character_t      *longerChar
                 , const cost_matrices_2d_t   *costMatrix
                 ,       alignment_matrices_t *algnMats
                 ,       size_t                deltawh
                 ,       size_t                len_shorterChar
                 ,       size_t                longerChar_len
                 )
{
    // printf("algn_nw_limit_2d %d\n", deltawh);
    // fflush(stdout);

//    const elem_t *slongerChar, *sshorterChar;
    cost_t *curRow,
                 *algn_precalcMtx;

    DIR_MTX_ARROW_t  *dirMtx;

//    slongerChar  = longerChar->char_begin;
//    sshorterChar = shorterChar->char_begin;
    curRow      = algnMats->algn_costMtx;
    dirMtx      = algnMats->algn_dirMtx;

    // printf("before pre-calc \n");
    // fflush(stdout);
    algn_precalcMtx = algnMats->algn_precalcMtx;
    // printf("after  pre-calc \n");
    // fflush(stdout);

    // printf("before pre-calc alignment\n");
    // fflush(stdout);
    algnMtx_precalc_4algn_2d( algnMats, costMatrix, shorterChar ); // returns precalculated cost matrix (in matrices) computed using
                                                                   // character from shorterChar.
                                                                   // shorterChar bases will be column heads of that matrix
    //printf("after  pre-calc alignment\n");
    //fflush(stdout);

    // cost_model_type is 1 for affine, 0 for non-affine
    // 50s here and below seems to be a default starting value for the matrix width/height
    if (costMatrix->cost_model_type) {
        return algn_fill_plane_2_affine ( longerChar
                                        , algn_precalcMtx
                                        , longerChar_len
                                        , len_shorterChar
                                        , curRow
                                        , dirMtx
                                        , costMatrix
                                        , 50
                                        , (longerChar_len - len_shorterChar) + 50
                                        , deltawh
                                        , curRow + (2 * len_shorterChar)
                                        , curRow + (4 * len_shorterChar)
                                        );
    } else {
        return algn_fill_plane_2 ( longerChar
                                 , algn_precalcMtx
                                 , longerChar_len
                                 , len_shorterChar
                                 , curRow
                                 , dirMtx
                                 , costMatrix
                                 , 50
                                 , (longerChar_len - len_shorterChar) + 50
                                 , deltawh
                                 );
    }
}

/** TODO: can probably eliminate either this or algn_nw_limit_2d, since
 *  both seem to be doing the same thing
 */
/** second character must be longer!!! */
cost_t
algn_nw_2d ( const dyn_character_t      *shorterChar
           , const dyn_character_t      *longerChar
           , const cost_matrices_2d_t   *costMatrix
           ,       alignment_matrices_t *algnMats
           ,       int                   deltawh
           )
{
    // deltawh is the size of the direction matrix, and was determined by the following algorithm:
    // let dif = longerCharlen - shorterCharlen
    // let lower_limit = longerCharlen * .1
    // if deltawh has no value
    //    then if dif < lower_limit
    //            then (lower_limit / 2)
    //            else 2
    //    else if dif < lower_limit
    //            then lower_limit
    //            else v

    // m and costMatrix must be allocated on other side of FFI.

    // At this point, gap is already set at beginning of char.
    // Bases are set as bit streams, with gap as most-significant bit.
    if(DEBUG_NW) {
        printf( "---algn_nw_2d\n" );
        printf( "first character\n" );
        dyn_char_print( longerChar );
        dyn_char_print( shorterChar );
        printf( "second character\n" );
        //algnMtx_print( algnMats, costMatrix->costMatrixDimension );
    }


    int longerChar_len  = longerChar->len,
        shorterChar_len = shorterChar->len;

    assert (longerChar_len >= shorterChar_len);
    return algn_nw_limit_2d ( shorterChar
                            , longerChar
                            , costMatrix
                            , algnMats
                            , deltawh
                            , shorterChar_len
                            , longerChar_len
                            );
}


int
algn_calculate_from_2_aligned ( dyn_character_t    *char1
                              , dyn_character_t    *char2
                              , cost_matrices_2d_t *costMatrix
                              , cost_t       *matrix
                              )
{
    // printf("algn_calculate_from_2_aligned\n");
    size_t i,
           gap_row = 0;
    int    res     = 0;

    cost_t gap_opening = costMatrix->gap_open_cost;

    elem_t gap_char    = costMatrix->gap_char,
           char1_begin,
           char2_begin;

    /* We initialize i to the proper location */
    char1_begin = char1->char_begin[0];
    char2_begin = char2->char_begin[0];
    if (   (  costMatrix->include_ambiguities
              && (gap_char &  char1_begin)
              && (gap_char &  char2_begin))
        || ( !costMatrix->include_ambiguities
              && (gap_char == char1_begin)
              && (gap_char == char2_begin)) )
    {
        i = 1;
    } else {
        i = 0;
    }
    assert ((char1->len) == (char2->len));
    for (; i < char1->len; i++) {
        char1_begin = char1->char_begin[i];
        char2_begin = char2->char_begin[i];
        if (0 == gap_row) { /* We have no gaps */
            if (   ( costMatrix->include_ambiguities
                     &&  (char1_begin & gap_char)
                     && !(char2_begin & gap_char))
                || (!costMatrix->include_ambiguities
                     && (char1_begin == gap_char)) )
            {
                res     += gap_opening;
                gap_row  = 1;
            }
            else if (   (  costMatrix->include_ambiguities
                           &&  (char2_begin &  gap_char)
                           && !(char1_begin & gap_char))
                     || ( !costMatrix->include_ambiguities
                           && (char2_begin == gap_char)) )
            {
                res     += gap_opening;
                gap_row  = 2;
            }
        }
        else if (1 == gap_row) { /* We are in char1's block of gaps */
            if (   ( costMatrix->include_ambiguities
                     && !(char1_begin &  gap_char))
                || (!costMatrix->include_ambiguities
                     && (char1_begin != gap_char)) )
            {

                if (   ( costMatrix->include_ambiguities
                         &&  (char2_begin &  gap_char)
                         && !(char1_begin & gap_char))
                    || ( !costMatrix->include_ambiguities
                          && (char2_begin == gap_char)) ) {
                  res    += gap_opening;
                  gap_row = 2;
                }
                else gap_row = 0;
            }
        }
        else { /* We are in char2's block of gaps */
            assert (2 == gap_row);
            if (   (  costMatrix->include_ambiguities
                      && !(char2_begin &  gap_char))
                || ( !costMatrix->include_ambiguities
                      && (char2_begin != gap_char)))
            {

                if (  ( costMatrix->include_ambiguities
                           && (char1_begin &  gap_char))
                      || ( !costMatrix->include_ambiguities
                           && (char1_begin == gap_char))) {
                    res    += gap_opening;
                    gap_row = 1;
                }
                else {
                    gap_row = 0;
                }
            }
        }
        res += cm_calc_cost_2d ( matrix
                               , char1->char_begin[i]
                               , char2->char_begin[i]
                               , costMatrix->alphSize
                               );
    }
    return (res);
}

/*
int
algn_worst_2 (dyn_character_t *char1, dyn_character_t *char2, cost_matrices_2d_t *c) {
    return (algn_calculate_from_2_aligned (char1, char2, c, c->worst));
}

int
algn_verify_2 (dyn_character_t *char1, dyn_character_t *char2, cost_matrices_2d_t *c) {
    return (algn_calculate_from_2_aligned (char1, char2, c, c->cost));
}
*/

void
algn_print_bcktrck_2d (const dyn_character_t *char1, const dyn_character_t *char2,
              const alignment_matrices_t *alignmentMatrices) {
    size_t i, j;
    DIR_MTX_ARROW_t *direction_matrix = alignmentMatrices->algn_dirMtx;
    printf ("\n");
    for (i = 0; i < char1->len; i++) {
        for (j = 0; j < char2->len; j++) {
            printf ("%u", direction_matrix[j]);
            fflush (stdout);
        }
        direction_matrix += j;
        printf ("\n");
        fflush (stdout);
    }
    printf ("\n\n");
    fflush (stdout);
}


void
algn_print_dynmtrx_2d ( const dyn_character_t      *char1
                      , const dyn_character_t      *char2
                      , const alignment_matrices_t *alignment_matrices
                      )
{

  int i, j;

  const int charLen1 = char1->len;
  const int charLen2 = char2->len;

  const dyn_character_t *longerChar  = charLen1 > charLen2 ? char1 : char2;
  const dyn_character_t *lesserChar  = charLen1 > charLen2 ? char2 : char1;

  const int longerCharLen = charLen1 > charLen2 ? charLen1 : charLen2;
  const int lesserCharLen = charLen1 > charLen2 ? charLen2 : charLen1;

  const int n = longerCharLen + 1;
  const int m = lesserCharLen + 1;

//  int *algn_costMatrix;
//  algn_costMatrix = alignment_matrices->algn_costMtx;

  DIR_MTX_ARROW_t *algn_dirMtx  = alignment_matrices->algn_dirMtx;

  printf ("Character 1 length: %d\n", charLen1);
  printf ("Character 2 length: %d\n", charLen2);
  printf ("Length    Product: %d\n", charLen1 * charLen2);
  printf ("Length +1 Product: %d\n", n * m);
  printf ("Allocated space  : %zu\n\n", alignment_matrices->cap_nw);

  /*
  printf("Cost matrix:\n");

  // print column heads

  printf("  x |    * ");
  for (i = 1; i < longerCharLen; i++) {
    printf("%4" elem_p " ", longerChar->char_begin[i]);
  }
  printf("\n");
  printf(" ---+-");

  for (i = 0; i < longerCharLen; i++) {
    printf("-----");
  }
  printf("\n");

  for (i = 0; i < lesserCharLen; i++) {
    if (i == 0) printf ("  * | ");
    else        printf (" %2" elem_p " | ", lesserChar->char_begin[i]);

    // We do this because only the last two "rows" are ever used and constantly
    // overwritten as the algorithm moves from the first "row" to the last row.
    // This means that only the last two "rows" are preserved in memory.
    // We place this loop here to pad out the unknown values.
    //
    // Also, the algorithm works on the transpose of the standard needleman-wunsh
    // matrix, so we actually access the "rows" as columns in the traditional
    // needleman-wunsh matrix with the longer character on the top and the shorter
    // on the left.
    //
    // The result of these design choices is that we can only see the last two
    // columns in the "proper" cost matrix. Hurray! /s
    for (j = 0; j < longerCharLen - 2; j++) {
      // if (j == 0 && i == 0) {
      //     printf("%7d ", 0);
      // } else {
      printf ("   ? ");
      // }
    }
    for (j = 0; j < 2; j++) {
      // if (j == 0 && i == 0) {
      //     printf("%7" cost_p " ", 0);
      // } else {
      printf ("%7" cost_p " ", algn_costMatrix[lesserCharLen * j + i]);
      // }
    }
    printf ("\n");
  }
  */

  // Print direction matrix
  setlocale(LC_CTYPE, "en_US.UTF-8");

  // wchar_t *name;
  printf("\n\nDirection matrix:\n");

  // print column heads
  printf("  x |    * ");
  for (i = 1; i < longerCharLen; i++) {
    printf("%4" elem_p " ", longerChar->char_begin[i]);
  }
  printf("\n");
  printf(" ---+-");

  for (i = 1; i < longerCharLen + 1; i++) {
    printf("-----");
  }
  printf("\n");

  for (i = 0; i < lesserCharLen; i++) {
    if (i == 0) printf ("  * | ");
    else        printf (" %2" elem_p " | ", lesserChar->char_begin[i]);

    for (j = 0; j < longerCharLen; j++) {
      DIR_MTX_ARROW_t dirToken = algn_dirMtx[lesserCharLen * i + j];

      printf("    "); // leading pad
      /**/
      printf("%s", dirToken & DELETE ? "<"  : " " );
      printf("%s", dirToken & ALIGN  ? "\\" : " " );
      printf("%s", dirToken & INSERT ? "^"  : " " );
      /**/
      /*
      // printf("%d", dirToken);
      wprintf(L"%wc", dirToken & DELETE ? (wchar_t *) "\u2191" : (wchar_t *) " ");
      wprintf(L"%wc", dirToken & ALIGN  ? (wchar_t *) "\u2196" : (wchar_t *) " ");
      wprintf(L"%wc", dirToken & INSERT ? (wchar_t *) "\u2190" : (wchar_t *) " ");
      */
      printf(" ");
    }
    printf ("\n");
  }

}


void
algn_string_of_2d_direction (DIR_MTX_ARROW_t v) {
    if (v & ALIGN   ) printf ("A");
    if (v & DELETE  ) printf ("D");
    if (v & INSERT  ) printf ("I");
    if (v & ALIGN_V ) printf ("VA");
    if (v & DELETE_V) printf ("VD");
    if (v & ALIGN_H ) printf ("HA");
    if (v & INSERT_H) printf ("HI");
}


void
algn_backtrace_2d ( const dyn_character_t *shorterChar
                  , const dyn_character_t *longerChar
                  , dyn_character_t *ret_longerChar
                  , dyn_character_t *ret_shorterChar
                  , const alignment_matrices_t *alignMatrix
                  , const cost_matrices_2d_t *costMatrix
                  , int st_longerChar
                  , int st_shorterChar
                  )
{
    int l,
        idx_longerChar,
        idx_shorterChar,
        new_item_for_ret_longerChar  = 0,
        new_item_for_ret_shorterChar = 0,
        shifter                      = 0;

    const char INDEL_GAP = 0;

    DIR_MTX_ARROW_t  *beg, *end;
    idx_longerChar  = longerChar->len;
    idx_shorterChar = shorterChar->len;
    l               = idx_longerChar * idx_shorterChar;
    beg             = alignMatrix->algn_dirMtx + st_shorterChar; // offset by limit into matrix
    // TODO: figure out wtf this means:
    /* Stitching goes to hell now
    end = beg + (idx_shorterChar * idx_longerChar) + idx_shorterChar - 1;
    */
    end = beg + (idx_longerChar * idx_shorterChar) - 1;
    l   = idx_shorterChar;

    if (DEBUG_ALGN) {
        printf("\nst_longerChar:   %d\n", st_longerChar);
        printf("st_shorterChar:  %d\n",   st_shorterChar);
        printf("idx_longerChar:  %d\n",   idx_longerChar);
        printf("idx_shorterChar: %d\n",   idx_shorterChar);

        if (DEBUG_DIR_M) {
            printf("\n");
            DIR_MTX_ARROW_t *beg_debug;
            int i, j;
            beg_debug = beg;

            printf ("Printing a two dimensional direction matrix.\n");
            for (i = 0; i < idx_longerChar; i++, beg_debug += idx_shorterChar) {
                for (j  = 0; j < idx_shorterChar; j++) {
                    algn_string_of_2d_direction (beg_debug[j]);
                    fprintf (stdout, "\t");
                    fflush (stdout);
                    end = beg_debug + j;
                }
                fprintf (stdout, "\n");
                fflush (stdout);
            }
            fprintf (stdout, "\n");
            fflush (stdout);
        }
    }

    end = beg + (idx_shorterChar * (idx_longerChar - 1)) + idx_shorterChar - 1;

    idx_longerChar  += st_longerChar;
    idx_shorterChar += st_shorterChar;

    if (!(costMatrix->cost_model_type)) { // not affine
        while (end >= beg) {
            if (*end & ALIGN) {
                idx_longerChar--;
                new_item_for_ret_longerChar = get_elem(longerChar, idx_longerChar);
                prepend_an_element(ret_longerChar, new_item_for_ret_longerChar);
                idx_shorterChar--;
                new_item_for_ret_shorterChar = get_elem(shorterChar, idx_shorterChar);
                prepend_an_element(ret_shorterChar, new_item_for_ret_shorterChar);
                end -= l + 1;
            }
            else if (*end & DELETE) {
                idx_longerChar--;
                new_item_for_ret_longerChar = get_elem(longerChar, idx_longerChar);
                prepend_an_element(ret_longerChar, new_item_for_ret_longerChar);
                new_item_for_ret_shorterChar = INDEL_GAP;
                prepend_an_element(ret_shorterChar, new_item_for_ret_shorterChar);
                end -= l;
            }
            else {
                assert (*end & INSERT);
                new_item_for_ret_longerChar = INDEL_GAP;
                prepend_an_element(ret_longerChar, new_item_for_ret_longerChar);
                idx_shorterChar--;
                new_item_for_ret_shorterChar = get_elem(shorterChar, idx_shorterChar);
                prepend_an_element(ret_shorterChar, new_item_for_ret_shorterChar);
                end -= 1;
            }
        }
    }
    else {             // affine
        while (end >= beg) {
            if (*end & (ALIGN << shifter)) {
                if (0 == shifter) {
                    idx_longerChar--;
                    new_item_for_ret_longerChar = get_elem(longerChar, idx_longerChar);
                    prepend_an_element(ret_longerChar, new_item_for_ret_longerChar);
                    idx_shorterChar--;
                    new_item_for_ret_shorterChar = get_elem(shorterChar, idx_shorterChar);
                    prepend_an_element(ret_shorterChar, new_item_for_ret_shorterChar);
                    end -= l + 1;
                }
                else if (SHIFT_V == shifter) {
                    idx_longerChar--;
                    new_item_for_ret_longerChar = get_elem (longerChar, idx_longerChar);
                    prepend_an_element(ret_longerChar, new_item_for_ret_longerChar);
                    new_item_for_ret_shorterChar = INDEL_GAP;
                    prepend_an_element(ret_shorterChar, new_item_for_ret_shorterChar);
                    end -= l;
                    shifter = 0;
                }
                else {
                    assert (SHIFT_H == shifter);
                    new_item_for_ret_longerChar = INDEL_GAP;
                    prepend_an_element(ret_longerChar, new_item_for_ret_longerChar);
                    idx_shorterChar--;
                    new_item_for_ret_shorterChar = get_elem(shorterChar, idx_shorterChar);
                    prepend_an_element(ret_shorterChar, new_item_for_ret_shorterChar);
                    end -= 1;
                    shifter = 0;
                }
            }
            else if (*end & (DELETE << shifter)) {
                if (0 == shifter) {
                    shifter = SHIFT_V;
                }
                else if (SHIFT_V == shifter) {
                    idx_longerChar--;
                    new_item_for_ret_longerChar = get_elem (longerChar, idx_longerChar);
                    prepend_an_element(ret_longerChar, new_item_for_ret_longerChar);
                    new_item_for_ret_shorterChar = 0;
                    prepend_an_element(ret_shorterChar, new_item_for_ret_shorterChar);
                    end -= l;
                }
                else {
                    assert (0);
                }
            }
            else {
                assert (*end & (INSERT << shifter));
                if (0 == shifter) {
                    shifter = SHIFT_H;
                }
                else if (SHIFT_H == shifter) {
                    new_item_for_ret_longerChar = INDEL_GAP;
                    prepend_an_element(ret_longerChar, new_item_for_ret_longerChar);
                    idx_shorterChar--;
                    new_item_for_ret_shorterChar = get_elem(shorterChar, idx_shorterChar);
                    prepend_an_element(ret_shorterChar, new_item_for_ret_shorterChar);
                    end -= 1;
                }
                else {
                    assert (0);
                }
            }
        } // end while
    }
}


void
algn_get_median_2d_with_gaps ( dyn_character_t *shorterChar
                             , dyn_character_t *longerChar
                             , cost_matrices_2d_t *costMatrix
                             , dyn_character_t *medianToReturn
                             )
{
    elem_t *char_begin_longerChar, *char_begin_shorterChar;
    elem_t interim;
    int i;
    char_begin_longerChar  = longerChar->char_begin;
    char_begin_shorterChar = shorterChar->char_begin;

    // printf("charLen - 1 of longer: %zu\n", longerChar->len - 1);

    for (i = longerChar->len - 1; i >= 0; i--) {
        elem_t x = char_begin_longerChar[i];
        elem_t y = char_begin_shorterChar[i];
        interim = cm_get_median_2d ( costMatrix
                                   // if we get a 0 value, use the gap_char value instead
				   // 0 value is used as an "output gap"
				   , x ? x : costMatrix->gap_char
				   , y ? y : costMatrix->gap_char
				   );
        dyn_char_prepend (medianToReturn, interim);
    }
}


void
algn_get_median_2d_no_gaps( dyn_character_t *shorterChar
                          , dyn_character_t *longerChar
                          , cost_matrices_2d_t *costMatrix
                          , dyn_character_t *sm
                          )
{
    elem_t       *char_begin_longerChar,
                 *char_begin_shorterChar,
                  interim;
    int           i;        // Can't be size_t, as counting down to 0.

    char_begin_longerChar  = longerChar->char_begin;
    char_begin_shorterChar = shorterChar->char_begin;
    for (i = longerChar->len - 1; i >= 0; i--) {
        elem_t x = char_begin_longerChar[i];
        elem_t y = char_begin_shorterChar[i];
        interim = cm_get_median_2d ( costMatrix
                                   // if we get a 0 value, use the gap_char value instead
				   // 0 value is used as an "output gap"
				   , x ? x : costMatrix->gap_char
				   , y ? y : costMatrix->gap_char
				   );

        if (interim != costMatrix->gap_char) {
            dyn_char_prepend (sm, interim);
        }
    }
    dyn_char_prepend (sm, costMatrix->gap_char); // TODO: Have to leave this here to deal with stupid extra gap at front.
}


void
algn_remove_gaps (elem_t gap_char, dyn_character_t *s)
{
    int i,                     // i and len must be signed for loop below to work.
        len    = (int) s->len,
        newlen = 0;

    elem_t *source,
           *destination;

    source = destination = s->end;

    for (i = len - 1; i >= 0; i--) {
        if (gap_char != *source) {
            *destination = *source;
            destination--;
            newlen++;
        }
        source--;
    }
    s->len = newlen;
    s->char_begin = destination + 1;
    /* We restore the leading gap */
    dyn_char_prepend (s, gap_char);
}


void
algn_correct_blocks_affine ( elem_t     gap_char
                           , dyn_character_t *s
                           , dyn_character_t *a
                           , dyn_character_t *b
                           )
{
    int i,
        len,
        extending_gap,
        inside_block = 0,
        prev_block   = 0;

    int aFirstOverlapGap,
        bFirstOverlapGap,
        sFirstOverlapGap;

    elem_t aFirst,
           bFirst,
           sFirst;

    len           = s->len;
    extending_gap = 0;
    inside_block  = 0;

    for (i = 0; i < len; i++) {
        aFirst = a->char_begin[i];
        bFirst = b->char_begin[i];
        sFirst = s->char_begin[i];

        aFirstOverlapGap = aFirst & gap_char;
        bFirstOverlapGap = bFirst & gap_char;
        sFirstOverlapGap = sFirst & gap_char;

        if (     !inside_block
              && (!aFirstOverlapGap || !bFirstOverlapGap) ) {
            inside_block = 0;
        } else if (   inside_block
                   && (!aFirstOverlapGap || !bFirstOverlapGap) ) {
            inside_block = 0;
        } else if (   (aFirstOverlapGap   || bFirstOverlapGap)
                   && (aFirst != gap_char || bFirst != gap_char) ) {
            inside_block = 1;
        } else {
            inside_block = 0;
        }

        if (     (aFirstOverlapGap || bFirstOverlapGap)
              && !sFirstOverlapGap
              && !extending_gap) {
            prev_block    = inside_block;
            extending_gap = 1;
        } else if (   aFirstOverlapGap
                   && bFirstOverlapGap
                   && sFirstOverlapGap
                   && (sFirst != gap_char)
                   && extending_gap
                   && inside_block
                   && !prev_block ) {
            sFirst     = ~gap_char & sFirst;
            prev_block = 0;
        } else if (aFirstOverlapGap && bFirstOverlapGap && extending_gap) {
            prev_block    = inside_block;
            extending_gap = 0;
        }

        dyn_char_set (s, i, sFirst);
    }

    algn_remove_gaps (gap_char, s);
}


inline void
algn_ancestor_2 ( dyn_character_t *char1
                , dyn_character_t *char2
                , cost_matrices_2d_t *costMatrix
                , dyn_character_t *medianToReturn
                )
{
    elem_t      *char_begin1,
                *char_begin2,
                 interim,
                 gap_char;

    int          are_ambiguities,
                 cost_model,
                 i;            // Can't be size_t, because counting down to 0.


    char_begin1     = char1->char_begin;
    char_begin2     = char2->char_begin;
    gap_char        = costMatrix->gap_char;
    are_ambiguities = costMatrix->include_ambiguities;
    cost_model      = costMatrix->cost_model_type;

    for (i = char1->len - 1; i >= 0; i--) {
        interim = cm_get_median_2d (costMatrix, char_begin1[i], char_begin2[i]);

        if (!are_ambiguities || cost_model != 1) {
            if (interim != gap_char) {
                dyn_char_prepend (medianToReturn, interim);
            }
        } else {
            dyn_char_prepend (medianToReturn, interim);
        }
    }
    if ( !are_ambiguities || (cost_model != 1 && gap_char != medianToReturn->char_begin[0])) {
        dyn_char_prepend (medianToReturn, gap_char);
    }
    else if (are_ambiguities) {
        algn_correct_blocks_affine (gap_char, medianToReturn, char1, char2);
    }
}


cost_t
algn_get_cost_medians_3d ( characters_t       *input
                         , cost_matrices_3d_t *costMatrix
                         , dyn_character_t    *ungapped_median
                         , dyn_character_t    *gapped_median
                         )
{
    elem_t interim;

    cost_t curCost = 0;

    if (DEBUG_3D) {
        printf("Get cost median:\n");
        printf("Seq length: %u\n", input->idxSeq1);
    }

    for (int i = input->idxSeq1 - 1; i >= 0; i--) {
        interim = cm_get_median_3d( costMatrix
                                  , input->seq1[i]
                                  , input->seq2[i]
                                  , input->seq3[i]
                                  );
        // if (DEBUG_3D)    printf("%d   %u\n", i, interim);

        // printVolumeOfMedianValues(costMatrix, 3, input->seq1[i], input->seq2[i], input->seq3[i]);

        dyn_char_prepend(gapped_median, interim);

        if (interim != costMatrix->gap_char) {
            dyn_char_prepend(ungapped_median, interim);
        }

        // NOTE that affine is not computed here.
        curCost += cm_get_cost_3d( costMatrix
                                 , input->seq1[i]
                                 , input->seq2[i]
                                 , input->seq3[i]
                                 );
    }

    return curCost;
}


void
algn_union ( dyn_character_t *shorterChar
           , dyn_character_t *longerChar
           , dyn_character_t *unionToReturn
           )
{
    assert (longerChar->len == shorterChar->len);
    assert (longerChar->cap >= shorterChar->len);
    int len, i;

    len = longerChar->len;

    for (i = len - 1; i >= 0; i--) {
        dyn_char_prepend( unionToReturn, (longerChar->char_begin[i] | shorterChar->char_begin[i]) );
    }
}
