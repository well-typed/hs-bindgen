// Test that when bool is given a definition (via #define), it is NOT parsed as
// PrimBool by language-c. In C23, bool is a keyword, but a macro definition
// should take precedence.

// This also requires _scoped macro types_. During reparse, we must only use
// macros that are currently in scope. In particular, `f` uses standard `bool`
// while `g` uses the macro `bool` which expands to `int` in this test case.

#define A int
void f(A x, bool y);

#define bool int
void g(A x, bool y);
