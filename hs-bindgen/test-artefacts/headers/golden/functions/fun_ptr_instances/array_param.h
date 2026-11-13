/* Array parameters of nested function types */

typedef int T;

// AdjustTypes adjusts the array parameter y to a pointer
void a(void (*x)(T y[3]));
