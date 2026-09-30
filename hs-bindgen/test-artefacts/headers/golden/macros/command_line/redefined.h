/*
 * Redefinition of a macro defined by a -D option after using it
 *
 * The test defines T on the command line.
 *
 * See <https://github.com/well-typed/hs-bindgen/issues/2280>.
 */

struct S { T x; };

#define T long
