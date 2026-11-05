#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alignCharacters.h"
#include "alignmentMatrices.h"
#include "c_alignment_interface.h"
#include "c_code_alloc_setup.h"
#include "debug_constants.h"
#include "dyn_character.h"
#include "ukkCommon.h"

/*
#define UNW_LOCAL_ONLY
#include <execinfo.h>
#include <signal.h>
#include <unistd.h>
#include <libunwind.h>


// Call this function to get a backtrace.
void backtrace_custom() {
  unw_cursor_t cursor;
  unw_context_t context;

  // Initialize cursor to current frame for local unwinding.
  unw_getcontext(&context);
  unw_init_local(&cursor, &context);

  // Unwind frames one by one, going up the frame stack.
  while (unw_step(&cursor) > 0) {
    unw_word_t offset, pc;
    unw_get_reg(&cursor, UNW_REG_IP, &pc);
    if (pc == 0) {
      break;
    }
    printf("0x%lx:", pc);

    char sym[256];
    if (unw_get_proc_name(&cursor, sym, sizeof(sym), &offset) == 0) {
      printf(" (%s+0x%lx)\n", sym, offset);
    } else {
      printf(" -- error: unable to obtain symbol name for this frame\n");
    }
  }
}


void handler(int sig) {
  void *array[10];
  size_t size;
  // get void*'s for all entries on the stack
  size = backtrace(array, 10);
  // print out all the frames to stderr
  fprintf(stderr, "Error: signal %d:\n", sig);
  backtrace_symbols_fd(array, size, STDERR_FILENO);

  char **strings;
  size_t i;

  strings = backtrace_symbols (array, size);
  printf ("Obtained %zd stack frames.\n", size);
  for (i = 0; i < size; i++)
     printf ("%s\n", strings[i]);

  free (strings);
  backtrace_custom();

  exit(1);
} 
*/


/** 
 * Takes three byte arrays, each of equal length. The length of the arrays are equal to 'allocationLen'.
 * Takes a pointer to an integer, this integer will store the resulting alignment length after execution.
 * Takes the lengths of the two input characters.
 **/
size_t cAlign2D
  ( elem_t             *lesserInput
  , elem_t             *longerInput
  , elem_t             *outputMedian
  , size_t             *outputLength
  , const size_t        allocationLen
  , const size_t        lesserLen
  , const size_t        longerLen
  , cost_matrices_2d_t *costMtx2d
  , const int           getUngapped
  , const int           getGapped
  , const int           getUnion
  )
{
  //signal(SIGSEGV, handler);
  //signal(SIGABRT, handler);
  
  /*
    if (DEBUG_ALGN) {
        printf("\n\nalign2d char1 input:\n");
        printf("\ninput char 1:");
        char_print(inputChar1_aio);
        printf("input char 2:");
        char_print(inputChar2_aio);
    }
*/

    size_t           alphabetSize  = costMtx2d->alphSize;
    /*** tmpLesserChar and tmpLongerChar will both have pointers into the input characters, 
         so don't need to be initialized separately 
     ***/
    dyn_character_t *tmpLesserChar = dyn_char_alloc(0);
    dyn_character_t *tmpLongerChar = dyn_char_alloc(0);

    stringToDynChar(tmpLesserChar, lesserInput, allocationLen, lesserLen, costMtx2d->gap_char);
    stringToDynChar(tmpLongerChar, longerInput, allocationLen, longerLen, costMtx2d->gap_char);

    if (DEBUG_ALGN) {
        printf("\nafter copying, char 1:\n");
        dyn_char_print(tmpLongerChar);
        printf("\nafter copying, char 2:\n");
        dyn_char_print(tmpLesserChar);
    }
    alignment_matrices_t *algnMtxs2d = malloc( sizeof(alignment_matrices_t) );
    assert( algnMtxs2d != NULL && "2D alignment matrices could not be allocated." );

    initializeAlignmentMtx( algnMtxs2d, tmpLongerChar->len, tmpLesserChar->len, alphabetSize );

    // deltawh is for use in Ukonnen, it gives the current necessary width of the Ukk matrix.
    // The following calculation to compute deltawh, which increases the matrix height or width in algn_nw_2d,
    // was pulled from POY ML code.
    int deltawh     = 0;
    int diff        = tmpLongerChar->len - tmpLesserChar->len;
    int lower_limit = .1 * tmpLongerChar->len;

    if (deltawh) {
        deltawh = diff < lower_limit ? lower_limit : deltawh;
    } else {
        deltawh = diff < lower_limit ? lower_limit / 2 : 2;
    }

    int algnCost = algn_nw_2d( tmpLesserChar, tmpLongerChar, costMtx2d, algnMtxs2d, deltawh );

    if (getGapped || getUngapped || getUnion) {
        //printf("Before backtrace.\n"), fflush(stdout);
      
        /*** Most character allocation is now done on Haskell side, 
             but these three are local, temporary buffers. 
         ***/
        dyn_character_t *retLesserChar = dyn_char_alloc(allocationLen);
        dyn_character_t *retLongerChar = dyn_char_alloc(allocationLen);
        dyn_character_t    *medianChar = dyn_char_alloc(allocationLen);
        
        algn_backtrace_2d( tmpLesserChar, tmpLongerChar, retLesserChar, retLongerChar, algnMtxs2d, costMtx2d, 0, 0 );

        if (getUngapped) {
            algn_get_median_2d_no_gaps(   retLesserChar, retLongerChar, costMtx2d, medianChar );
        }
        if (getGapped && !getUnion) {
            algn_get_median_2d_with_gaps( retLesserChar, retLongerChar, costMtx2d, medianChar );
        }
        if (getUnion) {
            algn_union(                   retLesserChar, retLongerChar, medianChar );
        }

        // Copy alignment results to output buffers
        memcpy( lesserInput, retLesserChar->array_head, allocationLen * sizeof(elem_t));
        memcpy( longerInput, retLongerChar->array_head, allocationLen * sizeof(elem_t));
        memcpy(outputMedian,    medianChar->array_head, allocationLen * sizeof(elem_t));
        *outputLength = medianChar->len - 1; // Subtract 1 for the prepended gap

        // Free temporary buffers
        dyn_char_free( retLongerChar );
        if (NULL != retLongerChar) free(retLongerChar);
        dyn_char_free( retLesserChar );
        if (NULL != retLesserChar) free(retLesserChar);
        dyn_char_free( medianChar );
        if (NULL != medianChar) free(medianChar);
    }

    freeNWMtx( algnMtxs2d );

    // Shouldn't have to free the whole structs here because they just pointed into the retChars.
    //dyn_char_free( tmpLongerChar );
    if (NULL != tmpLongerChar) free(tmpLongerChar);
    //dyn_char_free( tmpLesserChar );
    if (NULL != tmpLesserChar) free(tmpLesserChar);

    return algnCost;

}

/*
size_t cAlignAffine2D
  ( unsigned char      *lesser
  , unsigned char      *longer
  , unsigned char      *outputMedian
  , size_t             *outputLength
  , const size_t        allocationLen
  , const size_t        lesserLen
  , const size_t        longerLen
  , cost_matrices_2d_t *costMtx2d_affine
  , int                 getMedians
  );
{

    if (DEBUG_ALGN) {
        printf("\n\nalign2d char1 input:\n");
        printf("\ninput char 1:");
        alignIO_print(inputChar1_aio);
        printf("input char 2:");
        alignIO_print(inputChar2_aio);
    }

    const size_t allocationLen = inputChar1_aio->length + inputChar2_aio->length + 2; // 2 to account for gaps,
                                                                                      // which will be added in dyn_char_initialize()
    alignIO_t *longer,
              *lesser;

    // Most character allocation is now done on Haskell side, but these two are local.
    // tmpLongerChar and tmpLesserChar will both have pointers into the input characters, so don't need to be initialized separately
    dyn_character_t *tmpLongerChar     = dyn_char_alloc(0);
    dyn_character_t *tmpLesserChar    = dyn_char_alloc(0);
    dyn_character_t *retLongerChar  = dyn_char_alloc(allocationLen);
    dyn_character_t *retLesserChar = dyn_char_alloc(allocationLen);

    size_t alphabetSize = costMtx2d_affine->alphSize;

    if (inputChar1_aio->length >= inputChar2_aio->length) {
        stringToDynChar(tmpLongerChar, inputChar1_aio, alphabetSize);
        longer = inputChar1_aio;

        stringToDynChar(tmpLesserChar, inputChar2_aio, alphabetSize);
        lesser = inputChar2_aio;

    } else {
        stringToDynChar(tmpLongerChar, inputChar2_aio, alphabetSize);
        longer = inputChar2_aio;

        stringToDynChar(tmpLesserChar, inputChar1_aio, alphabetSize);
        lesser = inputChar1_aio;
    }

    if (DEBUG_ALGN) {
        printf("\nafter copying, char 1:\n");
        dyn_char_print(tmpLongerChar);
        printf("\nafter copying, char 2:\n");
        dyn_char_print(tmpLesserChar);
    }

    // TODO: document these variables
    // int *matrix;                        //
    cost_t *close_block_diagonal;       //
    cost_t *extend_block_diagonal;      //
    cost_t *extend_vertical;            //
    cost_t *extend_horizontal;          //
    cost_t *final_cost_matrix;          //
    cost_t *precalcMtx;                 //
    cost_t *matrix_2d;                  //
    cost_t *precalc_gap_open_cost;      // precalculated gap opening value (top row of nw matrix)
    cost_t *s_horizontal_gap_extension; //
    size_t        lenLongerChar;              //

    DIR_MTX_ARROW_t  *direction_matrix;

    alignment_matrices_t *algnMtxs2dAffine = malloc( sizeof(alignment_matrices_t) );
    assert( algnMtxs2dAffine != NULL && "Can't allocate 2D affine alignment matrices." );

    initializeAlignmentMtx(algnMtxs2dAffine, tmpLongerChar->len, tmpLesserChar->len, alphabetSize );
    // printf("Jut initialized alignment matrices.\n");
    lenLongerChar = tmpLongerChar->len;

    matrix_2d  = algnMtxs2dAffine->algn_costMtx;
    precalcMtx = algnMtxs2dAffine->algn_precalcMtx;


    algnMtx_precalc_4algn_2d( algnMtxs2dAffine, costMtx2d_affine, tmpLongerChar );


    // here and in algn.c, "block" refers to a block of gaps, so close_block_diagonal is the cost to
    // end a subcharacter of gaps, presumably with a substitution, but maybe by simply switching directions:
    // there was a vertical gap, now there's a horizontal one.

    // 2 through 11 below are offsets into various "matrices" in the alignment matrices, of which there are four
        of length 2 * longer_character and two of longer_character
    close_block_diagonal       =  matrix_2d;
    extend_block_diagonal      = (matrix_2d + ( lenLongerChar *  2 ));
    extend_vertical            = (matrix_2d + ( lenLongerChar *  4 ));
    extend_horizontal          = (matrix_2d + ( lenLongerChar *  6 ));
    final_cost_matrix          = (matrix_2d + ( lenLongerChar *  8 ));
    precalc_gap_open_cost      = (matrix_2d + ( lenLongerChar * 10 ));
    s_horizontal_gap_extension = (matrix_2d + ( lenLongerChar * 11 ));

    direction_matrix           = algnMtxs2dAffine->algn_dirMtx;

    // TODO: consider moving all of this into algn.
    //       the following three fns were initially not declared in algn.h
    algn_initialize_matrices_affine ( costMtx2d_affine->gap_open_cost
                          , tmpLesserChar
                          , tmpLongerChar
                          , costMtx2d_affine
                          , close_block_diagonal
                          , extend_block_diagonal
                          , extend_vertical
                          , extend_horizontal
                          , final_cost_matrix
                          , direction_matrix
                          , precalcMtx
                                    );

    int algnCost = algn_fill_plane_2d_affine( tmpLesserChar
                                  , tmpLongerChar
                                  , tmpLesserChar->len - 1  // -1 because of a loop condition in algn_fill_plane_2d_affine
                                  , tmpLongerChar->len  - 1  // -1 because of a loop condition in algn_fill_plane_2d_affine
                                  , final_cost_matrix
                                  , direction_matrix
                                  , costMtx2d_affine
                                  , extend_horizontal
                                  , extend_vertical
                                  , close_block_diagonal
                                  , extend_block_diagonal
                                  , precalcMtx
                                  , precalc_gap_open_cost
                                  , s_horizontal_gap_extension
                                            );

    if(getMedians) {
        dyn_character_t *ungappedMedianChar = dyn_char_alloc(allocationLen);
        dyn_character_t *gappedMedianChar   = dyn_char_alloc(allocationLen);

        algn_backtrace_affine( tmpLesserChar
                   , tmpLongerChar
                   , direction_matrix
                   , ungappedMedianChar
                   , gappedMedianChar
                   , retLesserChar
                   , retLongerChar
                   , costMtx2d_affine
                             );

        dynCharToString( ungappedOutput_aio, ungappedMedianChar, 1 );
        dynCharToString( gappedOutput_aio,   gappedMedianChar, 1 );

        dynCharToString( longer,  retLongerChar, 1 );
        dynCharToString( lesser, retLesserChar, 1 );

        dyn_char_free(ungappedMedianChar);
        if (NULL != ungappedMedianChar) free(ungappedMedianChar);
        dyn_char_free(gappedMedianChar);
        if (NULL != gappedMedianChar) free(gappedMedianChar);
    }

    freeNWMtx(algnMtxs2dAffine);
    // Can't free these structs internals because they're pointing into inputChar1_aio and inputChar2_aio
    // dyn_char_free(tmpLongerChar);
    // dyn_char_free(tmpLesserChar);
    if (NULL != tmpLongerChar) free(tmpLongerChar);
    if (NULL != tmpLesserChar) free(tmpLesserChar);
    dyn_char_free(retLongerChar);
    if (NULL != retLongerChar) free(retLongerChar);
    dyn_char_free(retLesserChar);
    if (NULL != retLesserChar) free(retLesserChar);

    return algnCost;
}
*/

/** Set `gap_open_cost` == `gap_extension_cost` for non-affine. */
// TODO: double check this
/*
cAlign3D( char          *inputChar1_aio
  , char          *inputChar2_aio
  , char          *inputChar3_aio
  , char          *outputChar1_aio
  , char          *outputChar2_aio
  , char          *outputChar3_aio
  , char          *ungappedOutput_aio
  , cost_matrices_3d_t *costMtx3d
  , cost_t        substitution_cost
  , cost_t        gap_open_cost // Set `gap_open_cost` == `gap_extension_cost` for non-affine. TODO: check this.
  , cost_t        gap_extension_cost
            )
{

    if (DEBUG_3D) {
        printf("\n\nalign3d input:\n");
        printf("\ninput char 1:");
        alignIO_print( inputChar1_aio );

        printf("input char 2:");
        alignIO_print( inputChar2_aio );

        printf("input char 3:");
        alignIO_print( inputChar3_aio );

        printf("\n\nalph size: %zu,   matrix dimension: %zu,   gap char: %u\n", costMtx3d->alphSize, costMtx3d->costMatrixDimension, costMtx3d->gap_char);
    }

    const size_t allocationLen = inputChar1_aio->length + inputChar2_aio->length + inputChar3_aio->length;

    cost_t algnCost;

    // powellInputs will be sent to Powell 3D alignment, powellOutputs will be returned.
    characters_t *powellInputs  = alloc_characters_t(inputChar1_aio->length, inputChar2_aio->length, inputChar3_aio->length);
    characters_t *powellOutputs = alloc_characters_t(allocationLen, allocationLen, allocationLen);

    alignIOtoCharacters_t( powellInputs, inputChar1_aio, inputChar2_aio, inputChar3_aio );

    if (DEBUG_CALL_ORDER) printf( "\n---Calling Powell\n\n" );

    // Powell aligns three sequences.
    algnCost = powell_3D_align ( powellInputs
                     , powellOutputs
                     , 4 // costMtx3d->alphSize
                     , substitution_cost   // mismatch cost, must be > 0
                     , gap_open_cost       // must be >= 0
                     , gap_extension_cost  // gap extension cost: must be > 0
                               );

    dyn_character_t *gappedMedianChar   = dyn_char_alloc( powellOutputs->idxSeq1 );
    dyn_character_t *ungappedMedianChar = dyn_char_alloc( powellOutputs->idxSeq1 );

    algnCost = algn_get_cost_medians_3d( powellOutputs
                             , costMtx3d
                             , ungappedMedianChar
                             , gappedMedianChar
                                       );

    dynCharToString( gappedOutput_aio,   gappedMedianChar,   0 );
    dynCharToString( ungappedOutput_aio, ungappedMedianChar, 0 );

    copyValsToAIO( outputChar1_aio, powellOutputs->seq1, powellOutputs->lenSeq1, powellOutputs->lenSeq1 );
    copyValsToAIO( outputChar2_aio, powellOutputs->seq2, powellOutputs->lenSeq1, powellOutputs->lenSeq1 );
    copyValsToAIO( outputChar3_aio, powellOutputs->seq3, powellOutputs->lenSeq1, powellOutputs->lenSeq1 );

    reverseCharacterElements(outputChar1_aio);
    reverseCharacterElements(outputChar2_aio);
    reverseCharacterElements(outputChar3_aio);
    reverseCharacterElements(gappedOutput_aio);
    reverseCharacterElements(ungappedOutput_aio);

    dyn_char_free(gappedMedianChar);
    if (gappedMedianChar != NULL) free(gappedMedianChar);
    dyn_char_free(ungappedMedianChar);
    if (ungappedMedianChar != NULL) free(ungappedMedianChar);

    free_characters_t(powellInputs);
    if (powellInputs != NULL) free(powellInputs);
    free_characters_t(powellOutputs);
    if (powellOutputs != NULL) free(powellOutputs);

    return algnCost;
}
*/


void stringToDynChar
  ( dyn_character_t *retChar
  ,       elem_t    *input
  , const size_t     capacity
  , const size_t     length
  , const elem_t     gap_elem
  )
{
    // Assign character into character struct. Note not copying, just creating pointers.
    size_t offset       = capacity - length;
    retChar->len        = length;
    retChar->cap        = capacity;
    retChar->array_head = input;
    retChar->end        = input + capacity - 1;
    retChar->char_begin = input + offset;

    // now add gap to beginning, Grrr.
    retChar->char_begin--;   // Add another cell, prepended to the array
    *retChar->char_begin = gap_elem; //Prepend a gap to the array.
    retChar->len++;
}


/*
void alignIOtoCharacters_t( characters_t *output
                , alignIO_t    *inputChar1
                , alignIO_t    *inputChar2
                , alignIO_t    *inputChar3
                          )
{
    output->seq1 = realloc( output->seq1, inputChar1->length * sizeof(elem_t) );
    output->seq2 = realloc( output->seq2, inputChar2->length * sizeof(elem_t) );
    output->seq3 = realloc( output->seq3, inputChar3->length * sizeof(elem_t) );

    memcpy( output->seq1, inputChar1->character + inputChar1->capacity - inputChar1->length, inputChar1->length * sizeof(elem_t) );
    memcpy( output->seq2, inputChar2->character + inputChar2->capacity - inputChar2->length, inputChar2->length * sizeof(elem_t) );
    memcpy( output->seq3, inputChar3->character + inputChar3->capacity - inputChar3->length, inputChar3->length * sizeof(elem_t) );

    output->lenSeq1 = inputChar1->length;
    output->lenSeq2 = inputChar2->length;
    output->lenSeq3 = inputChar3->length;

    output->idxSeq1 = 0;
    output->idxSeq2 = 0;
    output->idxSeq3 = 0;
}
*/

/** Takes in an alignIO and a char. *Copies* values of character from end of char to end of alignIO->character, so output must already
 *  be alloc'ed.
 *  Also eliminates extra gap at beginning of character that was needed by legacy code, as well as 0 that Powell appends to end of
 *  character.
 */
void dynCharToString
  ( elem_t          *output
  , dyn_character_t *input
  , const int        allocationLen
  , const int        delete_initial_gap
  )
{

    if (DEBUG_ALGN) {
        printf("input:\n");
        printf("  Length:   %zu\n", input->len);
        printf("  Capacity: %zu\n", input->cap);
        fflush(stdout);
    }

    size_t  copy_length;    // These two because ungapped characters will have their initial gaps removed, so may be length 0.
    elem_t *input_begin;

    // Now set length to copy and copy initial read location.
    // If input length > 0, Decrement because of the leading gap.
    // Ungapped character already has had initial gap removed.
    // Likewise, copy start has different setting for two conditions.
    if (input->len == 0) {
        copy_length = 0;
        input_begin = input->char_begin;
    } else if (delete_initial_gap) {
        copy_length = input->len - 1;
        input_begin = input->char_begin + 1;
    } else {
        copy_length = input->len;
        input_begin = input->char_begin;
    }
    size_t offset  =  - copy_length; // How far into output to start copying input character.         
    // Start copy after unnecessary gap char in input, if it exists.
    memcpy( output + offset, input_begin, copy_length * sizeof(elem_t) );
}

