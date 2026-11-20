/* Function types with a typedef of void as result type do not get instances */

typedef void V;
typedef int T;

// g is bound, but its nested function type does not get instances
void g(V (*x)(void));

// Typedef of a function type: F is bound, but does not get instances
typedef V F(void);

// A function type nested in one without instances still gets them
void k(V (*x)(void (*y)(T)));
