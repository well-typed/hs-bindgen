#include <stdio.h>

#include "foo.h"

/* Body */

extern int foo_default_body ();
extern int foo_hidden_body ();
extern int foo_protected_body ();
extern int foo_internal_body ();

/* Override */

int bar() {
  return -2;
}

int baz() {
  return -3;
}

int main() {
  printf("%d\n", foo());

  /* Header & Body */

  printf("%d\n", foo_default());
  // printf("%d\n", foo_hidden());
  // printf("%d\n", foo_protected());
  // printf("%d\n", foo_internal());

  /* Header */

  printf("%d\n", foo_default_header());
  printf("%d\n", foo_hidden_header());
  printf("%d\n", foo_protected_header());
  printf("%d\n", foo_internal_header());

  /* Body */

  printf("%d\n", foo_default_body());
  // printf("%d\n", foo_hidden_body());
  printf("%d\n", foo_protected_body());
  // printf("%d\n", foo_internal_body());

  /* Override */

  printf("%d\n", bar2());
  printf("%d\n", baz2());
}