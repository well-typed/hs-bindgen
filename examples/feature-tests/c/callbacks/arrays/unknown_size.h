/* callbacks with arrays of unknown size (by pointer to their first element) */

#include <stdlib.h>

typedef int list[];

typedef void list_op (list xs, size_t len);

extern void reverse_list (list xs, size_t len);

extern void apply_list_op (list_op * f, list xs, size_t len);
