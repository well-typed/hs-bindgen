/* A squashed typedef is defined in this module */

struct S { int x; };

// Squashed into struct S, which takes the name T
typedef struct S T;

// The argument type gets instances: T is defined in this module
void h(void (*x)(T *));
