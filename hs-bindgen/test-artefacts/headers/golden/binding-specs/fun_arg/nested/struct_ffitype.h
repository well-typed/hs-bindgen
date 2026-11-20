/* External binding of a struct, with an FFI type, in a nested function type */

struct S { int x; };

// Binding spec with ffitype
typedef struct S C;

// f is bound; its argument type does not get instances, and no message: none
// of its types is defined in this module
void f(void (*x)(C y));
