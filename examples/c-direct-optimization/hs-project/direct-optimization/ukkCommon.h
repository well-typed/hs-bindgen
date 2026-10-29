/*
 * Copyright (c) David Powell <david@drp.id.au>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 */


// Name:         ukkCommon.h
// FileType:     C Header
// Created:      Tue May 11 13:23:45 1999
// Author:       David Powell
//
// Contains the common routines for ukk.alloc, ukk.noalign, ukk.checkp and ukk.dpa
// Has the mem allocation routines, in header files because they are all inline funcs
// Use the NO_ALLOC_ROUTINES flag to compile without alloc routines.
// Use the FIXED_NUM_PLANES flag to make routines allocate on d^2 memory.

#ifndef __UKKCOMMON_H__
#define __UKKCOMMON_H__

#include <assert.h>
// #include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dyn_character.h"

#define MAX_STATES 27                 // Maximum number of possible states, 3^3

#define MAX_STR    100000   // TODO: can I get rid of this?
#define MAX_COST   (2 * MAX_STR)
#define INFINITY   INT_MAX / 2

typedef enum {match, del, ins} Trans;  // The 3 possible state-machine states

#ifndef __UKKCOMMON_C__

extern int misCost_g;
extern int startInsert_g;
extern int continueInsert_g;
extern int startDelete_g;
extern int continueDelete_g;

extern int neighbours[MAX_STATES];
extern int contCost[MAX_STATES];
extern int secondCost[MAX_STATES];
extern int transCost[MAX_STATES][MAX_STATES];
extern int stateNum[MAX_STATES];

extern int numStates_g;
extern int maxSingleStep_g;

extern elem_t gap_char_g;

// extern char Astr[MAX_STR];
// extern char Bstr[MAX_STR];
// extern char Cstr[MAX_STR];
// extern int Alen, Blen, Clen;
#endif // __UKKCOMMON_C_

#define maxSingleCost  (maxSingleStep_g * 2)


/** Holds arrays of characters along with their respective weights.
 *  Used for both input and output types for Ukkonnen alignment.
 */
typedef struct characters_t {
    elem_t *seq1;     // string representation inputs
    elem_t *seq2;     // string representation inputs
    elem_t *seq3;     // string representation inputs
    int     lenSeq1;  // lengths of A, B and Cstr. Have to be signed because compared to `furthestReached` in `ukkCheckPoint.c`
    int     lenSeq2;  // lengths of A, B and Cstr. Have to be signed because compared to `furthestReached` in `ukkCheckPoint.c`
    int     lenSeq3;  // lengths of A, B and Cstr. Have to be signed because compared to `furthestReached` in `ukkCheckPoint.c`
    int     idxSeq1;  // current location of pointer into Astr
    int     idxSeq2;  // current location of pointer into Bstr
    int     idxSeq3;  // current location of pointer into Cstr
} characters_t;


int whichCharCost( int a, int b, int c );
int okIndex( int a, int da, int end );

// Setup routines
int stateTransitionCost( int from, int to );


/** Mutates a, b, and c such that each is true or false if the least significant first, second or third digit, respectively,
 *  of neighbour is 1.
 *  I.e., if one of the transitions of the `neighbor` fsm is `delete`, set that state to true; otherwise, set to 0.
 */
void exists_neighbor_in_delete_state
  ( int  n
  , int *a
  , int *b
  , int *c
  );


int neighbourNum
  ( int i
  , int j
  , int k
  );


void transitions
  ( int s
  , Trans st[3]
  );


char *state2str( int s );


int countTrans
  ( Trans st[3]
  , Trans t
  );


void setup(void);


// Alignment checking routines
void checkAlign
  ( elem_t *al
  , int     alLen
  , elem_t *str
  , int     strLen
  );


/** As it says, reverses an array of `int`s */
void revIntArray
  ( int *arr
  , int  start
  , int  end
  );


/** As it says, reverses an array of `elem_t`s */
void revElem_tArray
  ( elem_t *arr
  , int     start
  , int     end
  );


/** Frees all internal pointers to a characters_t struct. */
void free_characters_t( characters_t *toFree );


/** Allocates all internal arrays and initializes array lengths and pointer locations for a characters_t struct with
 *  `number_elems` elements, where each element is of type `elem_t`, which is a synonym for `unsigned int`. */
characters_t *alloc_characters_t
  ( size_t seq_1_number_elems
  , size_t seq_2_number_elems
  , size_t seq_3_number_elems
  );


/** As it says, reverses an array of `char`s */
// TODO: This should be deprecated now, as we've switched entirely from chars to elem_ts
// void revCharArray( char *arr, int start, int end );


int alignmentCost
  ( int     states[]
  , elem_t *al1
  , elem_t *al2
  , elem_t *al3
  , int     len
  );


// typedef struct cost_state_vector_t
// {

// } cost_state_vector_t;


// #ifndef NO_ALLOC_ROUTINES
// Allocation stuff routines.  All inline -----------------------------------------

/*
  Memory organisation:

  basePtr -> pointer to array of pointers.  Array is indexed by edit cost,
             and contains a pointer to the plane for that cost.

  A Block is square containing CellsPerBlock x CellsPerBlock cells
    (note: each cell contains numStates_g states)

  A plane is 2d array of pointers to Blocks.  Enough pointers to cover
  from ab = - |B|..|A| and ac = - |C|..|A|.  Each block is only allocated as needed.

*/

//#define FIXED_NUM_PLANES

#define FULL_ALLOC_INFO 0

#define CellsPerBlock   10

typedef struct allocInfo_t {
    int    elemSize;
    int    abSize;
    int    acSize;
    int    abOffset;
    int    acOffset;
    long   abBlocks;
    int    acBlocks;
    int    costSize;
    long   baseAlloc;
    void **basePtr;
    long   memAllocated;
} AllocInfo_t;

//U_cell_type **Umatrix;        /* 2 dimensional */

// aInfo = allocMatrix(sizeof(U_cell_type));
//void allocUmatrix() {
//  UcostSize = maxSingleCost*2;
//  Umatrix = (U_cell_type **)allocMatrix(sizeof(U_cell_type *));
//}


void *allocEntry( AllocInfo_t *a );


static inline AllocInfo_t allocInit
  ( int           elemSize
  , int           costSize
  , characters_t *inputs
  )
{
    AllocInfo_t a;

    a.memAllocated = 0;
    a.elemSize     = elemSize;

    a.abSize   = inputs->lenSeq1 + inputs->lenSeq2 + 1;
    a.acSize   = inputs->lenSeq1 + inputs->lenSeq3 + 1;
    a.abOffset = inputs->lenSeq2;
    a.acOffset = inputs->lenSeq3;

    a.abBlocks = a.abSize / CellsPerBlock + 1;
    a.acBlocks = a.acSize / CellsPerBlock + 1;

// #ifdef FIXED_NUM_PLANES
    a.costSize  = costSize;
    a.baseAlloc = costSize;
// #else
//     a.baseAlloc = 20;        // Whatever not really important, will increase as needed
// #endif

    a.memAllocated += a.baseAlloc * sizeof(void *);
    a.basePtr       = calloc( a.baseAlloc, sizeof(void *) );
    assert( a.basePtr != NULL && "AllocInit: Unable to alloc memory." );

    return a;
}


static inline long allocGetSubIndex
  ( AllocInfo_t *a
  , int          ab
  , int          ac
  , int          s
  )
{
    long index = 0;

    int i = (ab + a->abOffset) / CellsPerBlock;
    int j = (ac + a->acOffset) / CellsPerBlock;
    int abAdjusted = ab + a->abOffset - i * CellsPerBlock;
    int acAdjusted = ac + a->acOffset - j * CellsPerBlock;

    //  fprintf(stderr, "ab = %d ac = %d abA = %d acA = %d abO = %d acO = %d i = %d j = %d\n",
    //      ab, ac, abAdjusted, acAdjusted, a->abOffset, a->acOffset, i, j);

    assert( abAdjusted >= 0 && abAdjusted < CellsPerBlock );
    assert( acAdjusted >= 0 && acAdjusted < CellsPerBlock );
    assert( s >= 0  && s < numStates_g );

    index = (index + abAdjusted) * CellsPerBlock;
    index = (index + acAdjusted) * numStates_g;
    index = (index + s);

    return index;
}


static inline void *allocPlane( AllocInfo_t *a )
{
    void *p;

    a->memAllocated += a->abBlocks * a->acBlocks * sizeof(void*);
    p = calloc(a->abBlocks * a->acBlocks, sizeof(void*));
    assert( p != NULL && "allocPlane: Unable to alloc memory." );

    return p;
}


// /** Main driver function */
// int doUkk( characters_t *inputs
//          , characters_t *outputs
//          );


/** recalloc - does a realloc() but sets any new memory to 0. */
static inline void *recalloc
  ( void  *p
  , size_t oldSize
  , size_t newSize
  )
{
    void *ptr = realloc(p, newSize);
    assert( ptr != NULL && "Failure in recalloc." );

    p = ptr;
    if (!p || oldSize > newSize) return p;

    memset( ((char*) p) + oldSize, 0, newSize - oldSize );
    return p;
}


static inline void *getPtr
  ( AllocInfo_t *a
  , int          ab
  , int          ac
  , int          d
  , int          s
  )
{
    int    i, j;
    void **bPtr;
    void  *base;
    int    index;

// #ifdef FIXED_NUM_PLANES
    // If doing a noalign or checkp,  remap 'd' into 0 .. costSize-1
    d = d % a->costSize;
// #endif

  // Increase the base array as needed
    while (d >= a->baseAlloc) {
        int oldSize = a->baseAlloc;
        a->baseAlloc *= 2;
        a->basePtr = recalloc(a->basePtr, oldSize*sizeof(void *), a->baseAlloc*sizeof(void*));
        assert( a->basePtr != NULL && "getPtr: Unable to alloc memory." );
        a->memAllocated += oldSize * sizeof(void*);
    }
    assert(d >= 0 && d < a->baseAlloc);

    if (a->basePtr[d] == NULL) {
        a->basePtr[d] = allocPlane(a);
    }
    bPtr = a->basePtr[d];

    i = (ab + a->abOffset) / CellsPerBlock;
    j = (ac + a->acOffset) / CellsPerBlock;
    assert(i >= 0 && i < a->abBlocks);
    assert(j >= 0 && j < a->acBlocks);

    if (bPtr[(i * a->acBlocks) + j] == NULL) {
        bPtr[(i * a->acBlocks) + j] = allocEntry(a);
    }

    base = bPtr[(i * a->acBlocks) + j];
    assert(base != NULL);

    index = allocGetSubIndex(a, ab, ac, s);
    assert(index >= 0);

    //  fprintf(stderr, "getPtr(ab = %d, ac = %d, d = %d, s = %d): base = %p index = %d\n",
    //          ab, ac, d, s,
    //          base, index);

    return ((char*) base) + (index * a->elemSize);
}

static inline void allocFinal
  ( AllocInfo_t *a
  , void        *flag
  , void        *top
  )
{
    int usedFlag = (char *) flag - (char *) top;
    {
        long planesUsed  = 0;
        long blocksTotal = 0, blocksUsed = 0;
        long cellsTotal  = 0, cellsUsed = 0;
        for (long i = 0; i < a->baseAlloc; i++) {
            long tblocksUsed = 0;
            void **p = a->basePtr[i];

            if (!p)   continue;

            planesUsed++;

            for (long j = 0; j<a->abBlocks * a->acBlocks; j++) {
                long tcellsUsed = 0;
                void *block = p[j];
                blocksTotal++;
                if (!block) continue;
                blocksUsed++;
                tblocksUsed++;
                for (int cIndex = 0; cIndex < CellsPerBlock * CellsPerBlock * numStates_g; cIndex++) {
                    cellsTotal++;
                    if ( *(int *) ( ((char *) block) + (cIndex * a->elemSize) + usedFlag) ) {
                        cellsUsed++;
                        tcellsUsed++;
                    }
                }
                if (OUTPUT_FINAL_ALLOC)  printf("Block %zu. Cells = %d Used = %ld\n", j, CellsPerBlock * CellsPerBlock * numStates_g, tcellsUsed);
            }
            if (OUTPUT_FINAL_ALLOC)  printf("Plane %zu. Blocks = %ld Used = %ld\n", i, a->abBlocks * a->acBlocks, tblocksUsed);
        }
        if (OUTPUT_FINAL_ALLOC) {
            printf("Total planes %ld, used %ld (used %ld bytes)\n", a->baseAlloc, planesUsed,
                   planesUsed * a->abBlocks * a->acBlocks * sizeof(void*));
            printf("Total blocks %ld, used %ld (%.2f%%) (used %ld bytes)\n",
                   blocksTotal, blocksUsed, (100.0 * blocksUsed / blocksTotal),
                   blocksUsed * CellsPerBlock * CellsPerBlock * numStates_g * a->elemSize);
            printf("Total cells %ld, used %ld (%.2f%%)\n",
                   cellsTotal, cellsUsed, (100.0 * cellsUsed / cellsTotal));
            printf("Total memory allocated = %ld bytes\n", a->memAllocated);
            printf("Approximation of actual mem used = %ld bytes\n",
                   (planesUsed * a->abBlocks * a->acBlocks * sizeof(void*)) + (cellsUsed * a->elemSize));
        }
    }
}
// #endif // NO_ALLOC_ROUTINES


/** This is entrance fn for Powell's 3d alignment code.
 *
 *  Note that alphabet size does not include gap.
 */
int powell_3D_align
  ( characters_t *inputSeqs     // lengths set correctly; indices set to 0
  , characters_t *outputSeqs    // lengths set correctly; indices set to 0
  , size_t        alphabetSize  // not including gap
  , int           mm            // mismatch cost, must be > 0
  , int           go            // gap open cost, must be >= 0
  , int           ge            // gap extension cost, must be > 0
  );

#endif // __UKK_COMMON_H__
