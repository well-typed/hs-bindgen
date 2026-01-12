/* Typedefs of structs */
// NOTE: all declarations are selected, unless otherwise specified.

struct MyStruct { int x; };
void foo  (struct MyStruct x);

typedef struct MyStruct A; // no binding spec
typedef A B; // no binding spec

void fooA (A x);
void fooB (B x);

// _X, _Y and _Z are intentionally distinct typedefs from A and B. We want C, D,
// and E to refer to unselected typedefs.

struct _X { int x; };
typedef struct _X _Y; // unselected
typedef _Y _Z; // unselected
typedef _Z C; // binding spec
typedef C D; // binding spec
typedef C E; // no binding spec

void fooC (C x);
void fooD (D x);
void fooE (E x);
