/*
 * Function pointers
 */

extern int square(int);

extern int cube (int) __attribute__ ((const));

extern int plus(int, int);

extern int apply1 (int (*f)(int), int x);

extern int apply2 (int (*f)(int, int), int x, int y);
