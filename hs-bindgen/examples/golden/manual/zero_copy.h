#pragma once

#include <stdint.h>

/*
 * Zero-copy bindings
 */

/* -------------------------------------------------------------------------- */
/* Structs */

struct point {
  int x;
  int y;
};

struct rectangle {
  struct point topleft;
  struct point bottomright;
};

struct circle {
  struct point midpoint;
  int radius;
};

/* -------------------------------------------------------------------------- */
/* Unions */

union shape {
  struct rectangle rectangle;
  struct circle circle;
};

/* -------------------------------------------------------------------------- */
/* Bit-fields */

// These bit-widths are completely arbitrarily chosen for illustratory purposes
struct colour {
  unsigned int opacity : 2;
  unsigned int brightness : 3;
  unsigned int red : 8;
  unsigned int green : 8;
  unsigned int blue : 8;
};

/* -------------------------------------------------------------------------- */
/* Typedefs */

typedef int myInt;

/* -------------------------------------------------------------------------- */
/* Pointers */

typedef struct drawing {
  union shape* shape;
  struct colour* colour;
} drawing;

/* -------------------------------------------------------------------------- */
/* Arrays */

typedef struct tic_tac_toe {
  int row1[3];
  int row2[3];
  int row3[3];
} tic_tac_toe;

/* -------------------------------------------------------------------------- */
/* FLAM */

struct vector {
  int len;
  char data[];
};

int reverse (const struct vector * input, struct vector * output);

/* -------------------------------------------------------------------------- */
/* Multi-dimensional arrays (and typedefs) */

typedef int triplet[3];
typedef triplet matrix[3];

void transpose (const matrix input, matrix output);
