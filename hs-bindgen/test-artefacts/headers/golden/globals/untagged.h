/* -------------------------------------------------------------------------- */
/* global variables that reference untagged/struct/enum/union types */

// hs-bindgen generates getter C functions for global variables. This getter
// needs special handling if a global variable's type references an untagged
// struct/union/enum: we can't pretty-print such C types because there is no tag
// to refer to. In such cases we pretty-print void* instead.

/* -------------------------------------------------------------------------- */
/* basic examples */

struct { int x; int y; } anonPoint;
// with initializer
struct { int a; int b; } anonPair = { 1, 2 };

enum { VAL_A = 0, VAL_B = 1 } anonEnum;
// with initializer
enum { X = 10, Y = 20, Z = 30 } anonEnumCoords = { X };

/* -------------------------------------------------------------------------- */
/* examples with indirections */

// array-of-untagged-enum
enum { a1 } A[17];

// const-untagged-union
const union { int x; } B;

// pointer-to-array-of-const-pointer-to-untagged-struct
struct { int x; } * const (*C)[];
