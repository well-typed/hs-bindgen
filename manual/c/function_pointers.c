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

/*
 * Implicit function to pointer conversion
 */

int apply1_pointer_arg (int2int * f, int x) {
  return f(x);
}

int apply1_nopointer_arg (int2int f, int x) {
  return f(x);
}

int (* const apply1_nopointer_res (void)) (int2int, int) {
  return &apply1_nopointer_arg;
}

int (* const apply1_nopointer_var)(int2int, int) = &apply1_nopointer_arg;

const struct Apply1Struct apply1_struct = {
    .apply1_nopointer_struct_field = &apply1_nopointer_arg
  };

const union Apply1Union apply1_union = {
    .apply1_nopointer_union_field = &apply1_nopointer_arg
  };
