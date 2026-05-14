/* Const-qualified arrays */

/* of unknown size */

void foo  (const int x[]);

typedef const int S[];
typedef       int T[];

void fooA (      S x);
void fooB (const S x);
void fooC (const T x);

/* of known size */

void bar  (const int x[3]);

typedef const int U[3];
typedef       int V[3];

void barA (      U x);
void barB (const U x);
void barC (const V x);

/* in a pointer */

void baz  (const int (*x)[3]);

typedef const int W[3];
typedef       int X[3];

void bazA (      W * x);
void bazB (const W * x);
void bazC (const X * x);
