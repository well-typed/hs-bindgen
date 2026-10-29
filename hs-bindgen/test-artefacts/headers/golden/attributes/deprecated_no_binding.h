// Deprecated declarations for which we generate no binding of their own

// A deprecated typedef, squashed into its struct
typedef struct { int x; } point __attribute__((deprecated));

// A deprecated function that fails to parse
void f(long double x) __attribute__((deprecated));
