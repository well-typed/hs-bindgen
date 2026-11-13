/* External binding without an FFI type in a nested function type */

// Binding spec without ffitype
typedef int C;

typedef int T;

// Instances for the argument type, with a warning
void f(void (*x)(C y, T z));
