/* Typedefs of enums */
// NOTE: all declarations are selected, unless otherwise specified.

enum MyEnum {x};
void foo  (enum MyEnum x);

typedef enum MyEnum A; // no binding spec
typedef A B; // no binding spec

void fooA (A x);
void fooB (B x);

// _X, _Y and _Z are intentionally distinct typedefs from A and B. We want C, D,
// and E to refer to unselected typedefs.

enum _X {y};
typedef enum _X _Y; // unselected
typedef _Y _Z; // unselected
typedef _Z C; // binding spec
typedef C D; // binding spec
typedef C E; // no binding spec

void fooC (C x);
void fooD (D x);
void fooE (E x);
