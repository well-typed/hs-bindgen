/**
 * Conflicting attributes on functions for llvm/clang versions 18 and up
 *
 * Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html
 */

// const and pure conflict

int square_cp (int x) __attribute__ ((const, pure));

int square_pc (int x) __attribute__ ((pure, const));

int square_cc (int x) __attribute__ ((const, const));

int square_pp (int x) __attribute__ ((pure, pure));