#include <stdio.h>

extern int foo(int x);
extern int foo_default(int x);
extern int foo_hidden (int x);
extern int foo_protected(int x);
extern int foo_internal(int x);

int main() {
  printf("%d\n", foo(2));
  printf("%d\n", foo_default(3));
  printf("%d\n", foo_hidden(4));
  printf("%d\n", foo_protected(5));
  printf("%d\n", foo_internal(6));
}