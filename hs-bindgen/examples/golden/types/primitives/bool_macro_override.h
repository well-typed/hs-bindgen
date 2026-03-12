// Test that when bool is given a definition (via #define), it is NOT parsed as
// PrimBool by language-c. In C23, bool is a keyword, but a macro definition
// should take precedence.

#define A int
void f(A x, bool y);

#define bool int
void g(A x, bool y);
