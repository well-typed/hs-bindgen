/* Typedefs of functions */
// NOTE: all declarations are selected, unless otherwise specified.

void foo  (int x (int));

typedef int A(int); // no binding spec
typedef A B; // no binding spec

void fooA (A x);
void fooB (B x);

// _Y and _Z are intentionally distinct typedefs from A and B. We want C, D,
// and E to refer to unselected typedefs.

typedef int _Y(int); // unselected
typedef _Y _Z; // unselected
typedef _Z C; // binding spec
typedef C D; // binding spec
typedef C E; // no binding spec

void fooC (C x);
void fooD (D x);
void fooE (E x);

// foo* and bar* style functions are equivalent, but we add them both for
// completeness.

void bar  (int (*x) (int));

void barA (A * x);
void barB (B * x);

void barC (C * x);
void barD (D * x);
void barE (E * x);
