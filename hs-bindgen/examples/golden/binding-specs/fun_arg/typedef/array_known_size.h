/* Typedefs of arrays of known size */
// NOTE: all declarations are selected, unless otherwise specified.

void foo  (int x[3]);

typedef int A[3]; // no binding spec
typedef A B; // no binding spec

void fooA (A x);
void fooB (B x);

// _Y and _Z are intentionally distinct typedefs from A and B. We want C, D, and
// E to refer to unselected typedefs.

typedef int _Y[3]; // unselected
typedef _Y _Z; // unselected
typedef _Z C; // binding spec
typedef C D; // binding spec
typedef C E; // no binding spec

void fooC (C x);
void fooD (D x);
void fooE (E x);
