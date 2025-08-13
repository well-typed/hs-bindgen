#include "globals.h"

// Global variables

int externGlobalInt = 9;

// Global constants

const int globalConstant = 1;
ConstInt anotherGlobalConstant = 12;

const int constArray1 [4] = {1, 2, 3, 4};
const int constArray2 [] = {1, 2, 3, 4, 5};

const struct tuple constTuple = { 3, 7};
struct tuple nonConstTuple = { 3, 5};

const int * ptrToConstInt; // empty-inialised to 0
int * const constPtrToInt; // empty-inialised to 0
const int * const constPtrToConstInt; // empty-inialised to 0
