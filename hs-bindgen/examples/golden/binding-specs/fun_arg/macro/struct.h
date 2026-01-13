/* Macros of structs */
// NOTE: all declarations are selected, unless otherwise specified.

struct MyStruct { int x; }; // no binding spec
void foo  (struct MyStruct x);

#define A struct MyStruct // no binding spec
#define B A // no binding spec

void fooA (A x);
void fooB (B x);

/* TODO: temporarily disabled
// _X, _Y and _Z are intentionally distinct macros from A and B. We want C, D,
// and E to refer to unselected macros.

struct _X { int x; }; // unselected
#define _Y struct _X // unselected
#define _Z _Y // unselected
#define C _Z // binding spec
#define D C // binding spec
#define E C // no binding spec

void fooC (C x);
void fooD (D x);
void fooE (E x);
*/
