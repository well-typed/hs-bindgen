// Two _different_ declarations of the same macro T. This should result in a
// warning.
#define T int
void foo (T x);
#define T char
void bar (T x);
