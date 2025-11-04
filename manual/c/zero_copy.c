#include "zero_copy.h"

/* -------------------------------------------------------------------------- */
/* FLAM */

int reverse (const struct vector * input, struct vector * output) {
  int i;
  int n = input->len;
  if (n != output->len) return -1;
  for (i = 0; i < n; i++)
    output->data[i] = input->data[n-1-i];
  return 0;
};

/* -------------------------------------------------------------------------- */
/* Multi-dimensional arrays (and typedefs) */

void transpose (const matrix input, matrix output) {
  int i, j;
  for (i = 0; i < 3; i++)
    for (j = 0; j < 3; j++)
      output[i][j] = input[j][i];
};