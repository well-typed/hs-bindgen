/* Macros of arrays of unknown size */
// NOTE: all declarations are selected, unless otherwise specified.

// Not replaced by external binding specs
typedef int MyArray[];
void foo(MyArray x);

// Not replaced by external binding specs
#define A MyArray
#define B A

void fooA(A x);
void fooB(B x);

// Test transitive dependencies of macros replaced by external binding
// specifications

// _X, _Y and _Z are intentionally distinct macros from A and B. We want C, D,
// and E to refer to unselected macros.

// Not selected
typedef int _X[];
#define _Y _X
#define _Z _Y

// Replaced by external binding specs
#define C _Z
#define D C

// Not replaced by external binding spec
#define E C

void fooC(C x);
void fooD(D x);
void fooE(E x);
