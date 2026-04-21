#pragma once

/*
 * Global variables
 */

 struct globalConfig {
  int numThreads;
  int numWorkers;
};

extern struct globalConfig globalConfig;

/*
 * Non-extern globals
 */

int nonExternGlobalInt = 8;

/*
 * Unsupported: thread-local variables
 */

// NOTE: We test the error for thread_local separately (as it requires C23).

/*
 * Anonymous declarations
 */

extern struct {
  int x;
  int y;
} unusableAnon;

/*
 * Constants
 */

extern const int globalConstant;
typedef const int ConstInt;
static ConstInt anotherGlobalConstant = 123;

/*
 * Constants: more examples
 */

//! An array of known size of const ints
extern const int constArray1 [4];
//! An array of unknown size of const insts
extern const int constArray2 [];

struct tuple { int x; const int y; };
//! A constant tuple
extern const struct tuple constTuple;
//! A non-constant tuple with a constant member
extern struct tuple nonConstTuple;

//! An int
extern int Int;
//! A const int
extern const int constInt;
//! A pointer to int
extern int * ptrToInt;
//! A pointer to const int
extern const int * ptrToConstInt;
//! A const pointer to int
extern int * const constPtrToInt;
//! A const pointer to const int
extern const int * const constPtrToConstInt;

/*
 * Guidelines for binding generation
 */

extern int a;
extern const int a2;

extern int * b;
extern const int * b2;

typedef int triplet[3];
extern triplet c;
extern const triplet c2;

typedef int list[];
extern list d;
extern const list d2;


