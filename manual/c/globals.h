#pragma once

// Global variables

int globalInt = 8;
extern int externGlobalInt;

// Global constants

//! Although this is a constant, we don't expect an initializer since it's
// extern.
extern const int globalConstant;

//! Constant with typedef
typedef const int ConstInt;
extern ConstInt anotherGlobalConstant;

//! Constant, but local to the file
//
// Unlike with extern, in this we _do_ expect an initializer.
static const int staticConst = 123;

//! No storage class specified
const int classless;
