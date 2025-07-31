#include "arrays.h"

#include <stdio.h>

/*
 * Global variables
 */

int arr2[3] = {2, 3, 4};

int arr3[] = {3, 4, 5};


int sudoku[3][3] = { {9, 8, 7}
                   , {6, 5, 4}
                   , {3, 2, 1}
                   };

int triplets[][3] = { {0, 0, 0}
                    , {1, 1, 1}
                    };

triplet baz = {9,1,8};

/*
 * Matrix transpose
 */

void transpose (matrix input, matrix output) {
  int i, j;
  for (i = 0; i < 3; i++)
      for (j = 0; j < 3; j++)
          output[i][j] = input[j][i];
};

/*
 * Complex examples
 */

int (*global_triplet_ptrs[])[3] = { &triplets[0], &triplets[1], &triplets[0] };

void pretty_print_triplets (int (*ts[])[3]) {
  // Pointer to the first triplet
  triplet *t1 = ts[0];
  // Pointer to the second triplet
  triplet *t2 = ts[1];
  // Pointer to the third triplet
  triplet *t3 = ts[2];
  printf("{%d, %d, %d}\n", (*t1)[0], (*t1)[1], (*t1)[2]);
  printf("{%d, %d, %d}\n", (*t2)[0], (*t2)[1], (*t2)[2]);
  printf("{%d, %d, %d}\n", (*t3)[0], (*t3)[1], (*t3)[2]);
}
