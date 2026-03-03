#include "unknown_size.h"

// https://stackoverflow.com/a/199891
void reverse_list (list xs, size_t len) {
    int c, i, j;
    for (i = 0, j = len - 1; i < j; i++, j--)
    {
        c = xs[i];
        xs[i] = xs[j];
        xs[j] = c;
    }
};

void apply_list_op (list_op * f, list xs, size_t len) {
  return f(xs, len);
};
