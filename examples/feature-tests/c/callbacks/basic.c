#include "basic.h"

int square (int x) {
  return (x * x);
}

int apply_int_op (int_op * f, int x) {
  return f(x);
}
