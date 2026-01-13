/* Macros of unions */
// NOTE: all declarations are selected, unless otherwise specified.

union MyUnion { int x; }; // no binding spec
void foo  (union MyUnion x);

#define A union MyUnion // no binding spec
#define B A // no binding spec

void fooA (A x);
void fooB (B x);

/* TODO: temporarily disabled
// _X, _Y and _Z are intentionally distinct macros from A and B. We want C, D,
// and E to refer to unselected macros.

union _X { int x; }; // unselected
#define _Y union _X // unselected
#define _Z _Y // unselected
#define C _Z // binding spec
#define D C // binding spec
#define E C // no binding spec

void fooC (C x);
void fooD (D x);
void fooE (E x);
*/
