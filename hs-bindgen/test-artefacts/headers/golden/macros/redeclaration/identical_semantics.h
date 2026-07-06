// Two _semantically_ identical declarations of the same macro.
#define T int
void foo (T x);
#define T int
void bar (T x);
