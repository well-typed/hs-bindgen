// Two _different_ declarations of the same macro T, but with an undef in
// between. This should result in a warning.
#define T int
void foo (T x);
#undef T
#define T char
void bar (T x);
