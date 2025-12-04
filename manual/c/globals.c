#include "globals.h"

/*
 * Global variables
 */

struct globalConfig globalConfig = {5, 8};

/*
 * Constants
 */

const int globalConstant = 1;

/*
 * Constant examples
 */

const int constArray1 [4] = {1, 2, 3, 4};
const int constArray2 [] = {1, 2, 3, 4, 5};

const struct tuple constTuple = { 3, 7};
struct tuple nonConstTuple = { 3, 5};

int Int = 8;
const int constInt = 9;
int * ptrToInt = &Int;
const int * ptrToConstInt = &constInt;
int * const constPtrToInt = &Int; // cheating with a cast
const int * const constPtrToConstInt = &constInt;

/*
 * Guidelines for binding generation
 */

int a = 17;
const int a2 = 18;

int * b = &a;
const int * b2 = &a;

triplet c = {19, 20, 21};
const triplet c2 = {22, 23, 24};

typedef int list[];
list d = {25, 26, 27, 28};
const list d2 = {29, 30};
