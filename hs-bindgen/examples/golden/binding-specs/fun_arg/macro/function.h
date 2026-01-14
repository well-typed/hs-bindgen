/* Macros of functions */
// NOTE: all declarations are selected, unless otherwise specified.

typedef int MyFunction (int); // no binding spec
void foo  (MyFunction x);

#define A MyFunction // no binding spec
#define B A // no binding spec

void fooA (A x);
void fooB (B x);

/* TODO: can be enabled once the 'TransitiveDependenciesMissing' warning is
fixed. See issue #1513.

// _X, _Y and _Z are intentionally distinct macros from A and B. We want C, D,
// and E to refer to unselected macros.

typedef int _X(int); // unselected
#define _Y _X // unselected
#define _Z _Y // unselected
#define C _Z // binding spec
#define D C // binding spec
#define E C // no binding spec

void fooC (C x);
void fooD (D x);
void fooE (E x);
*/

// foo* and bar* style functions are equivalent, but we add them both for
// completeness.

void bar  (int (*x) (int));

void barA (A * x);
void barB (B * x);

/* TODO: can be enabled once the 'TransitiveDependenciesMissing' warning is
fixed. See issue #1513.

void barC (C * x);
void barD (D * x);
void barE (E * x);
*/
