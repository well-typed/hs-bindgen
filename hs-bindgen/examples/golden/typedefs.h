typedef int myint;
typedef int * intptr;

// Unsupported function with a typedef type. See issue #1034.
typedef int int2int(int);
extern int2int foo;

typedef void (*FunctionPointer_Function)(void);
typedef int (NonFunctionPointer_Function)(int value);

typedef void (*f1)(void);

typedef void g1(void);
typedef g1 * g2;

typedef void h1(void);
typedef h1 h2;
typedef h2 * h3;
