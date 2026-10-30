// A type macro used under a pointer by a macro defined after it
#define T int
#define P T *

// A type macro used under a pointer by a macro defined before it
#define Z_PTR A_T *
#define A_T int
