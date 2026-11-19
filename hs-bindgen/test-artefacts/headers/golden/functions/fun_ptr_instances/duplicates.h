/* Function types with the same instance head */

typedef int T;

// Differ only in a qualifier
void d(void (*a)(const T), void (*b)(T));

// A function parameter and a function pointer parameter
void e(void (*a)(void b(T)), void (*c)(void (*b)(T)));
