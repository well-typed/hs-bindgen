#include "known_size.h"
#include "unknown_size.h"

void reverse_vec5 (vec5 xs) {
  reverse_list(xs, 5);
};

void apply_vec5_op (vec5_op * f, vec5 xs) {
  return f(xs);
};
