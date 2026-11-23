/* Function types passing a struct by value do not get instances */

struct S { int x; };
typedef struct S T;

// h is bound, but its argument type does not get instances
void h(T (*x)(void));
