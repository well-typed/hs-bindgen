#pragma once

// Global variables

int globalInt = 8;
extern int externGlobalInt;

/**
 * Constants
 */

//! Constant
//!
//! Although this is a constant, we don't expect an initializer (since it's
//! `extern`).
extern const int globalConstant;

//! Constant, through typedef
typedef const int ConstInt;
extern ConstInt anotherGlobalConstant;

//! Constant, but local to the file
//!
//! Unlike with `extern`, in this we _do_ expect an initializer.
static const int staticConst = 123;

//! No storage class specified
const int classless;

//! A an array of size 4 containing constant integers
extern const int constArray1 [4];

//! An array of uknown size containing constant integers
typedef const int ConstIntArray[];
extern ConstIntArray constArray2;

struct tuple { int x; const int y; };
//! A constant tuple
extern const struct tuple constTuple;
//! A non-constant tuple with a constant member
extern struct tuple nonConstTuple;

//! A pointer to const int
extern const int * ptrToConstInt;
//! A const pointer to int
extern int * const constPtrToInt;
//! A const pointer to const int
extern const int * const constPtrToConstInt;
