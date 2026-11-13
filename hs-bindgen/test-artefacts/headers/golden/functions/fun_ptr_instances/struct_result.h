/* Function types passing a struct by value get no instances */

struct S { int x; };
typedef struct S T;

// h is bound, without instances for its argument type
void h(T (*x)(void));
