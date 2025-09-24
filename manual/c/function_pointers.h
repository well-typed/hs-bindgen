/*
 * Function pointers
 */

extern int square(int);

extern int plus(int, int);

extern int apply1 (int (*f)(int), int x);

extern int apply2 (int (*f)(int, int), int x, int y);

/*
 * Implicit function to pointer conversion
 */

//! A version of apply1() that declares to take a argument of function type, but
//! this function type is automatically adjusted to the corresponding pointer type.
extern int apply1_nopointer (int f(int), int x);
