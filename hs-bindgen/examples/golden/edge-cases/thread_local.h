/**
 * Thread local global variables
 *
 * By default we generate a pointer for global variables, but that may not be
 * safe for thread-local variables. We therefore do not generate any bindings
 * for these currently (https://github.com/well-typed/hs-bindgen/issues/828).
 *
 * NOTE: This is a C23 feature, and requires llvm >= 16.
 */
thread_local extern int threadLocal;
