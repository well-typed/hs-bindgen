typedef int myint;
typedef int * intptr;

// Unsupported function with a typedef type. See issue #1034.
typedef int int2int(int);
extern int2int foo;
