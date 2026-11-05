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

#ifndef DYN_CHAR_H
#define DYN_CHAR_H

#include <stddef.h>

/**
 *  The encoding type for "sequence elements."
 *
 *  NOTE:
 *    -  Always use 'elem_t' as the type for seuqence elements
 *    -  Always use 'elem_p' within the formatting string for 'printf' calls.
 */
#ifndef TYPE_OF_ELEM
#define TYPE_OF_ELEM

#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>
#define elem_p PRIu32
typedef uint32_t elem_t;

#endif /* TYPE_OF_ELEM */


/* Dynamic character structure to be used inside ocaml custom types. */
/********************* CHARACTER AS IT COMES IN MUST BE IN LAST X SPACES IN ARRAY! *********************/
typedef struct dyn_character_t {
//    int magic_number;
    size_t cap;         // Capacity of the character memory structure.
    size_t len;         // Total length of the character stored.
    elem_t *array_head; // beginning of the allocated array
    elem_t *char_begin; // Position where the first element of the character is actually stored.
    elem_t *end;        // End of array. Last element in character is at end - 1 so that prepending works correctly.
    //struct pool *my_pool; ARRAY_POOL_DELETE
} dyn_character_t;


/** Does internal allocation for a character struct. Also sets character pointers within array to correct positions.
 *
 *  resChar must be alloced before this call. This is because allocation must be done on other side of FFI for pass
 *  by ref to be correct.
 *
 *  Note that it allocates allocSize + 1, because prepending won't work if array end == character end.
 */
dyn_character_t *dyn_char_alloc( size_t allocSize );


void dyn_char_free( dyn_character_t *toFree );


/** Adds v to the front of the character array inside a. Increments the length of a and decrements the pointer to the head of
 *  the character.
 */
void dyn_char_prepend
  ( dyn_character_t *a
  , elem_t           v
  );


void dyn_char_print( const dyn_character_t *inChar );


/* Stores the value v in the position p of character a. */
void dyn_char_set
  ( dyn_character_t *character
  , size_t           position
  , elem_t           value
  );

#endif /* DYN_CHAR_H */
