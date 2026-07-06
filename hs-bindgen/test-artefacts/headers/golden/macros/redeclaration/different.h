// Two _different_ declarations of the same macro T.
#define T int
void foo (T x);
#define T char
void bar (T x);
