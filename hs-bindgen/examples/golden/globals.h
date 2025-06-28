/**
 * Various kinds of globals
 */

// Simple global variable
extern int simpleGlobal;

// // Constant
// //
// // Although this is a constant, we don't expect an initializer (since it's
// // `extern`).
// extern const int globalConstant;

// // Constant, through typedef
// typedef const int ConstInt;
// extern ConstInt anotherGlobalConstant;

// // Constant, but local to the file
// //
// // Unlike with `extern`, in this we _do_ expect an initializer.
// static const int staticConst = 123;

// extern struct { int x; int y; } foo;

// /**
//  * Error cases
//  */

// // Without `extern` or `static`, this declares a /new/ variable.
// //
// // This kind of declaration should not appear in header files, as it would
// // result in duplicate symbols (and when they do, we should ignore them).
// int notActuallyGlobal;

// // No storage class specified
// //
// // Even though this is declared `const`, this is still an error (for the same
// // reason as `notActuallyGlobal`).
// const int classless;

// // We don't currently support thread_local variables
// thread_local int threadLocal;

