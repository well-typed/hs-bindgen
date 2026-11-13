/* Function types with the same instance head */

typedef int T;

// Differ only in a qualifier
void d(void (*a)(const T), void (*b)(T));

// Parameter of function type against one of function pointer type
void e(void (*a)(void b(T)), void (*c)(void (*b)(T)));
