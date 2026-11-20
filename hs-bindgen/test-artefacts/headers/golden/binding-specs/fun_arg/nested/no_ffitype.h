/* External binding without an FFI type in a nested function type */

// Binding spec without ffitype
typedef int C;

typedef int T;

// The argument type gets instances, with a warning
void f(void (*x)(C y, T z));
