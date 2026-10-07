// Clang treats malloc and strlen as builtins. From LLVM/Clang 22 on, their
// size_t argument and result have the type __size_t. We bind them with the
// underlying unsigned long, as with LLVM/Clang 21 and earlier.

#include <stddef.h>

void *malloc(size_t size);
size_t strlen(const char *s);

// Not a builtin, so size_t stays a typedef
size_t my_strlen(const char *s);
