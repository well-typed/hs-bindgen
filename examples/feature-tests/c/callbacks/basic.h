/* callbacks with basic types */

typedef int int_op (int);

extern int square(int);

extern int apply_int_op (int_op * f, int x);
