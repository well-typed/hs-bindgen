#define A int
#define A int
#define ID(A, ...) A __VA_ARGS__
typedef ID(const, int) T;
