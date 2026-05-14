// Test that when bool is given a definition (via typedef), it is NOT parsed as
// PrimBool by language-c.

#define A int
typedef int bool;
void f(A x, bool y);
