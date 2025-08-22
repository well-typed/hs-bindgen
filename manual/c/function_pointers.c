#include "function_pointers.h"
#include <stdio.h>

int square (int x) {
  return (x * x);
}

int plus (int x, int y) {
  return (x + y);
}

int apply1 (int (*f)(int), int x) {
  return (*f)(x);
}

int apply2 (int (*f)(int, int), int x, int y){
  return (*f)(x, y);
}
