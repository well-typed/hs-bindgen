#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alignmentMatrices.h"
#include "c_code_alloc_setup.h"
#include "costMatrix.h"
#include "debug_constants.h"


/** Find distance between an ambiguous nucleotide and an unambiguous ambElem. Return that value and the median.
 *  @param ambElem is ambiguous input.
 *  @param nucleotide is unambiguous.
 *  @param median is used to return the calculated median value.
 *
 *  This fn is necessary because there isn't yet a cost matrix set up, so it's not possible to
 *  look up ambElems, therefore we must loop over possible values of the ambElem
 *  and find the lowest cost median.
 *
 *  Requires symmetric, if not metric, matrix.
 */
int distance
  ( cost_t const *tcm
  , size_t alphSize
  , elem_t nucleotide
  , elem_t ambElem
  )
{
    int min     = INT_MAX;
    int curCost = 0;

    for (size_t pos = 0; pos < alphSize; pos++) {
        if ((1 << pos) & ambElem) { // if pos is set in ambElem, meaning pos is possible value of ambElem
            curCost = (int) tcm[pos * alphSize + nucleotide];
            if (curCost < min) {
                min = curCost;
            }
        }
    }
    return min;
}


void freeChar(dyn_character_t *toFree) {
    if (NULL != toFree->array_head) free(toFree->array_head);
    if (NULL != toFree) free(toFree);
}


void freeCostMtx(void * input, int is_2d) {

    if (is_2d) {
        if (NULL != ((cost_matrices_2d_t *) input)->cost)         free( ((cost_matrices_2d_t *) input)->cost);
        if (NULL != ((cost_matrices_2d_t *) input)->median)       free( ((cost_matrices_2d_t *) input)->median);
        if (NULL != ((cost_matrices_2d_t *) input)->worst)        free( ((cost_matrices_2d_t *) input)->worst);
        if (NULL != ((cost_matrices_2d_t *) input)->prepend_cost) free( ((cost_matrices_2d_t *) input)->prepend_cost);
        if (NULL != ((cost_matrices_2d_t *) input)->tail_cost)    free( ((cost_matrices_2d_t *) input)->tail_cost);
    } else {
        if (NULL != ((cost_matrices_3d_t *) input)->cost)   free( ((cost_matrices_3d_t *) input)->cost);
        if (NULL != ((cost_matrices_3d_t *) input)->median) free( ((cost_matrices_3d_t *) input)->median);
    }

    if (NULL != input) free (input);
}


/**
 *
 * TODO: make sure I'm actually deallocing right here.
 */
void freeNWMtx(alignment_matrices_t *input) {
    if (NULL != input->algn_costMtx)    free (input->algn_costMtx);
    if (NULL != input->algn_dirMtx)     free (input->algn_dirMtx);
    if (NULL != input->algn_precalcMtx) free (input->algn_precalcMtx);

    if (NULL != input) free(input);
}


void initializeAlignmentMtx( alignment_matrices_t *retMtx
                           , size_t                len_char1
                           , size_t                len_char2
                           , size_t                alphSize
                           )
{
    // printf("initializeAlignmentMtx\n");
    // in six following allocations all matrices are set to their shortest length because they get realloced in algnMat_setup_size
    retMtx->cap_nw          =  0; // a suitably small number to trigger realloc, but be larger than len_eff
    retMtx->cap_eff         =  0; // cap_eff was -1 so that cap_eff < cap, triggering the realloc
    retMtx->cap_pre         =  0; // again, trigger realloc

    retMtx->algn_costMtx    = malloc( sizeof( cost_t ) );
    retMtx->algn_dirMtx     = malloc( sizeof( DIR_MTX_ARROW_t ) );
    retMtx->algn_precalcMtx = malloc( sizeof( cost_t ) );
    /* don't have to allocate these two, because they're just pointing to algn_costMtx and algn_dirMtx.
    retMtx->cube          = malloc ( sizeof( int* ) );
    retMtx->cube_d        = malloc ( sizeof( int* ) );
    */
    assert(   retMtx->algn_costMtx    != NULL
           && retMtx->algn_dirMtx     != NULL
           && retMtx->algn_precalcMtx != NULL
           && "Can't allocate alignment matrices." );

    algnMat_setup_size (retMtx, len_char1, len_char2, alphSize);
}


void setUp2dCostMtx( cost_matrices_2d_t *retCostMtx
                   , cost_t const       *tcm
                   , cost_t             gap_open
                   , size_t             alphSize
                   )
{

    // first allocate retMatrix
    int    combinations = 1;                     // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int    do_aff       = gap_open == 0 ? 0 : 3; // The 3 is because affine's cost_model_type is 3, according to my reading of ML code.
                                                 // (Actually, I changed that; it used to be 2, now it's 3.)
                                                 // This value set in cm_set_affine().
    int    is_metric    = 1;
    elem_t all_elements = (1 << alphSize) - 1;   // Given data is DNA (plus gap), there are 2^5 - 1 possible character states

    int    minCost      = INT_MAX;
    elem_t median       = 0;                     // cumulative median for 2d; combo of median1, etc., below
    int    curCost;

    // int    median1, median2;                    // median of a given nucleotide and current ambElem, for each ambElem

    //    int tcm2[25] = {0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0};

    //    tcm = tcm2;

    cm_alloc_2d( retCostMtx
               , alphSize
               , combinations
               , do_aff
               , (int) gap_open
               , is_metric
               , all_elements
               );
    // Print TCM in pretty format
    if(DEBUG_MAT) {
        printf("setUp2dCostMtx\n");
        const size_t n = retCostMtx->costMatrixDimension;
        for (size_t i = 0; i < n; ++i) {
            for (size_t j = 0; j < n; ++j) {
                printf("%2" cost_p " ", tcm[ n * i + j ]);
            }
            printf("\n");
        }
    }

    for (elem_t ambElem1 = 1; ambElem1 <= all_elements; ambElem1++) { // for every possible value of ambElem1, ambElem2
        for (elem_t ambElem2 = 1; ambElem2 <= all_elements; ambElem2++) {
            //curCost = 0;                // don't actually need to do this
            minCost = INT_MAX;
            median  = 0;
            // median1 = median2 = 0;

            elem_t bitIndex;
            for ( bitIndex = 0; bitIndex < alphSize; bitIndex++) {
                curCost = distance (tcm, alphSize, bitIndex, ambElem1)
                        + distance (tcm, alphSize, bitIndex, ambElem2);
                // now seemingly recreating logic in distance(), but that was to get the cost for each
                // ambElem; now we're combining those costs get overall cost and median
                if (curCost < minCost) {
                    minCost = curCost;
                    median  = 1 << bitIndex; // median = this bitIndex, because it has the lowest cost thus far
                } else if (curCost == minCost) {
                    median |= 1 << bitIndex; // median = this bitIndex | old median
                }
            } // bitIndex
            cm_set_cost_2d   (retCostMtx, ambElem1, ambElem2, minCost);
            cm_set_median_2d (retCostMtx, ambElem1, ambElem2, median);
        } // ambElem2
    } // ambElem1

    /* TODO: which of following two loops is correct? */

    elem_t gap = retCostMtx->gap_char;
    for ( size_t i = 1; i <= all_elements; i++) {
        cm_set_prepend_2d (retCostMtx, i, cm_get_cost_2d(retCostMtx, gap,   i));
        cm_set_tail_2d    (retCostMtx, i, cm_get_cost_2d(retCostMtx,   i, gap));
    }

    /*
    elem_t* charStart = longChar->char_begin;
    int gap        = 1 << (alphSize - 1);
    int charElem;
    for ( size_t i = 0; i < longChar->len; i++) {
        charElem = (int) *(charStart + i);
        cm_set_prepend_2d (i, cm_get_cost(gap, charElem, retCostMtx), retCostMtx);
        cm_set_tail_2d    (cm_get_cost(charElem, gap, retCostMtx), i, retCostMtx);
    } */
//    return retCostMtx;
    if(DEBUG_COST_M) {
        printf("2d:\n");
        cm_print_2d (retCostMtx);
    }
}


void setUp3dCostMtx( cost_matrices_3d_t *retMtx
                   , cost_t const       *tcm
                   , cost_t             gap_open
                   , size_t             alphSize
                   )
{
    // first allocate retMatrix
    int combinations = 1;                     // false if matrix is sparse. In this case, it's DNA, so not sparse.
    int do_aff       = gap_open == 0 ? 0 : 3; // The 3 is because affine's cost_model_type is 3, according to my reading of ML code.
                                              // (Actually, I changed that; it used to be 2, now it's 3.)
                                              // This value set in cm_set_affine().
    // int is_metric    = 1;
    elem_t all_elements = (1 << alphSize) - 1;   // Given data is DNA (plus gap), for instance, there are 2^5 - 1 possible character states

    elem_t median = 0;        // and 3d; combos of median1, etc., below
    int minCost, curCost;

    cm_alloc_3d( retMtx
               , alphSize
               , combinations
               , do_aff
               , (int) gap_open
               , all_elements );

    for (elem_t ambElem1 = 1; ambElem1 <= all_elements; ambElem1++) { // for every possible value of ambElem1, ambElem2, ambElem3
        for (elem_t ambElem2 = 1; ambElem2 <= all_elements; ambElem2++) {
            for (elem_t ambElem3 = 1; ambElem3 <= all_elements; ambElem3++) {
                minCost = INT_MAX;
                median  = 0;
                for (elem_t bitIndex = 0; bitIndex < alphSize; bitIndex++) {
                    curCost = distance (tcm, alphSize, bitIndex, ambElem1)
                            + distance (tcm, alphSize, bitIndex, ambElem2)
                            + distance (tcm, alphSize, bitIndex, ambElem3);
                    if (curCost < minCost) {
                        minCost = curCost;
                        median  = ((elem_t) 1) << bitIndex;
                    }
                    else if (curCost == minCost) {
                        median |= ((elem_t) 1) << bitIndex;
                    }
                } // bitIndex
                // printf("%2" elem_p " %2" elem_p " %2" elem_p " %2" cost_p " %2" elem_p "\n", ambElem1, ambElem2, ambElem3, minCost, median);
                cm_set_cost_3d(   retMtx, ambElem1, ambElem2, ambElem3, minCost );
                cm_set_median_3d( retMtx, ambElem1, ambElem2, ambElem3, median  );
            } // ambElem3
        } // ambElem2
    } // ambElem1
   // return retMtx;
    if(DEBUG_COST_M) {
        printf("3d:\n");
        cm_print_3d (retMtx);
    }

}
