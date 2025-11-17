#include <stdbool.h>
#include <stddef.h>

// Outermost const; type is const-int.
extern const int a;
// No outermost const; type is pointer-to-const-int.
extern const int *b;
// Outermost const; type is const-pointer-to-int.
extern int *const c;
// Outermost const; type is const-pointer-to-const-int.
extern const int *const d;

// Type qualifier in function argument.
bool list_example(const char **items, size_t count);
