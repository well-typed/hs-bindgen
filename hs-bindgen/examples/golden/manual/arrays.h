#pragma once

/* Arrays
 *
 * See https://en.cppreference.com/w/c/language/array.html
 */

/*
 * Global variables
 */

//! Global, complete, initialised
extern int arr1[1];

//! Global, extern, complete, not initialised
extern int arr2[3];

//! Global, extern, incomplete
extern int arr3[];

//! Multi-dimensional array of known size.
extern int sudoku [3][3];

//! Multi-dimensional array of unknown size. Only the first dimension is allowed
//! to be unknown.
extern int triplets[][3];

/*
 * Matrix transpose
 */

typedef int triplet[3];

typedef triplet matrix[3];

void transpose (const matrix input, matrix output);

/*
 * Complex example
 */

//! A typedef representing a an array of unknown size, where each element is a
//! pointer to an array of known size 3, where each element is an int.
typedef int (*triplet_ptrs[])[3];

//! A global of triplet_ptrs
extern triplet_ptrs global_triplet_ptrs;

//! A function that prints the given triplet_ptrs
extern void pretty_print_triplets (triplet_ptrs x);
