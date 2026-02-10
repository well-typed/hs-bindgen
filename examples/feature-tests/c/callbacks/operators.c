#include "operators.h"

int square (int x) {
  return (x * x);
}

int plus (int x, int y) {
  return (x + y);
}

int apply1 (unary_op * f, int x) {
  return (*f)(x);
}

int apply2 (bin_op * f, int x, int y){
  return (*f)(x, y);
}
