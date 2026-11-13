/* Function types with a typedef of void as result type get no instances */

typedef void V;
typedef int T;

// Nested function type: g is bound, without instances for its argument type
void g(V (*x)(void));

// Typedef of a function type: F is bound, without instances
typedef V F(void);

// A function type nested in one without instances still gets them
void k(V (*x)(void (*y)(T)));
