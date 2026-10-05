/*
 * Declarations ending in a macro argument
 *
 * The expansion location of the last token of each declaration is the start of
 * the macro invocation; reparsing has to tokenise up to its end.
 */

#define T int
#define PARAMS(args) args
#define ID(x) x

T f PARAMS((T a));

typedef T A ID([3]);

struct S { T x ID([3]); };
