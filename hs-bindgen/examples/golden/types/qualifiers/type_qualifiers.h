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

// Multiple pointer levels with const qualifiers interleaved at various positions
// Type: pointer-to-const-pointer-to-const-int (const at two levels)
extern const int *const *e;
// Type: pointer-to-pointer-to-const-pointer-to-const-int (3 levels, const at innermost two)
extern const int *const **f;
// Type: pointer-to-const-pointer-to-pointer-to-const-int (3 levels, const at outer and inner)
extern const int **const *g;
// Type: const-pointer-to-const-pointer-to-const-pointer-to-const-int (4 levels, all const)
extern const int *const *const *const h;
// Type: pointer-to-const-pointer-to-pointer-to-const-pointer-to-int (4 levels, alternating const)
extern int *const **const *i;
// Type: pointer-to-pointer-to-const-pointer-to-const-pointer-to-const-int (5 levels, const at inner three)
extern const int *const *const **j;
