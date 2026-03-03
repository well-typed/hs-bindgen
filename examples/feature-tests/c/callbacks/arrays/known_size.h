/* callbacks with arrays of known size (by pointer to their first element) */

#include <stdlib.h>

typedef int vec5[5];

typedef void vec5_op (vec5 xs);

extern void reverse_vec5 (vec5 xs);

extern void apply_vec5_op (vec5_op * f, vec5 xs);
