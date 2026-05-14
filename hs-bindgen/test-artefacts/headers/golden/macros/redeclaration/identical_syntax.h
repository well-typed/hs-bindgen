// Two _syntactically_ identical declarations of the same macro T. Both
// definitions expand to macro A, but macro A has a different definition in the
// two cases. This should result in a warning.
#define A int
#define T A
void foo (T x);
#undef A
#define A char
#define T A
void bar (T x);
