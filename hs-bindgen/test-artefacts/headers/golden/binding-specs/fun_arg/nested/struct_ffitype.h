/* External binding of a struct, with an FFI type, in a nested function type */

struct S { int x; };

// Binding spec with ffitype
typedef struct S C;

// f is bound, without instances for its argument type: C is a struct
void f(void (*x)(C y));
