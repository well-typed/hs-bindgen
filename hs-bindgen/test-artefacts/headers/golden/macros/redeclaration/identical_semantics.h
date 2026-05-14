// Two _semantically_ identical declarations of the same macro. This should
// _NOT_ result in any error or warnings.
#define T int
void foo (T x);
#define T int
void bar (T x);
