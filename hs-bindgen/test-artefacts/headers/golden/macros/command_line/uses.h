/*
 * Uses of macros defined by -D options
 *
 * The test defines T, V and F on the command line.
 */

struct S { T x; };

#define W F(V)
