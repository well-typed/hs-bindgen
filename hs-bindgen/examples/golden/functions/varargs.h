// Test that we do not try to produce any bindings for variadic functions (but
// do continue to generate bindings for the other functions).

#include <stdarg.h>

void f(const char* fmt, ...);
void g(const char* fmt, va_list args);
void h();
