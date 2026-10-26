#ifndef C_ALIGNMENT_INTERFACE_H
#define C_ALIGNMENT_INTERFACE_H

#include "alignCharacters.h"
#include "alignmentMatrices.h"
#include "c_code_alloc_setup.h"
#include "costMatrix.h"
#include "debug_constants.h"


/** Do a 2d alignment. Depending on the values of last two inputs,
 *  | (0,0) = return only a cost
 *  | (0,1) = calculate gapped and ungapped characters
 *  | (1,0) = calculate union
 *  | (1,1) = calculate both union and ungapped characters.
 *
 *  In the last case the union will replace the gapped character placeholder.
 */
size_t cAlign2D
  (       elem_t             *lesser
  ,       elem_t             *longer
  ,       elem_t             *outputMedian
  ,       size_t             *outputLength
  , const size_t              allocationLen
  , const size_t              lesserLen
  , const size_t              longerLen
  ,       cost_matrices_2d_t *costMtx2d
  , const int                 getUngapped
  , const int                 getGapped
  , const int                 getUnion
  );


/** As align2d, but affine.
 *
 *  If `getMedians` gapped & ungapped outputs will be medians.
 */
size_t cAlignAffine2D
  (       elem_t       *lesser
  ,       elem_t       *longer
  ,       elem_t       *outputMedian
  ,       size_t       *outputLength
  , const size_t        allocationLen
  , const size_t        lesserLen
  , const size_t        longerLen
  , cost_matrices_2d_t *costMtx2d_affine
  ,      int            getMedians
  );


/** Aligns three characters using affine algorithm.
 *  Set `gap_open_cost` to equal `gap_extension_cost` for non-affine.
 *
 *  First declares, allocates and initializes data structures.
 *  Calls ukkCheckPoint.powell_3D_align().
 *  Calls alignCharacters.algn_get_cost_medians_3d().
 *  Copies output to correct return structures.
 *
 *  Ordering of inputs by length does not matter, as they will be sorted inside the fn.
 */
int cAlign3D
  ( elem_t             *inputChar1_aio
  , elem_t             *inputChar2_aio
  , elem_t             *inputChar3_aio
  , elem_t             *outputChar1_aio
  , elem_t             *outputChar2_aio
  , elem_t             *outputChar3_aio
  , elem_t             *ungappedOutput_aio
  , cost_matrices_3d_t *costMtx3d
  , cost_t              substitution_cost
  , cost_t              gap_open_cost
  , cost_t              gap_extension_cost
  );


/**  prints an alignIO struct */
void alignIO_print( const char *character );


/** For use in 2D alignment.
 *
 *  Takes in an `alignIO` struct and a `dyn_character` struct. Points interior pointers to input `alignIO`.
 *  Points `dyn_character->char_begin`, `dyn_character->end` to respective points in `alignIO->character`.
 *  Adds a gap character at the front of the array, to deal with old OCaml-forced interface.
 *
 *  The values in the last `length` elements in `input` get copied to the _last_ `length` elements in the array in`retChar`.
 *
 *  This _does not_ allocate of copy values.
 *
 *  Nota bene: assumes that retChar->character has already been allocated correctly.
 */
void stringToDynChar
  (       dyn_character_t *retChar
  ,       elem_t          *input
  , const size_t           capacity
  , const size_t           length
  , const elem_t           gap_elem
  );


/** For use in 2D alignment code.
 *
 *  Takes in an alignIO and a dynamic character. *Copies* values of character from end of dynamic character to end of alignIO->character,
 *  so output must already be alloc'ed.
 *
 *  Also eliminates extra gap needed by legacy code.
 */
void dynCharToString
  (       elem_t          *output
  ,       dyn_character_t *input
  , const int              allocationLen
  , const int              delete_initial_gap
  );


#endif // C_ALIGNMENT_INTERFACE_H
