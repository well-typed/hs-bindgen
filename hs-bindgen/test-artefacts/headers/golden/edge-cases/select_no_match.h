// Test case for SelectNoDeclarationsMatched warning
//
// This file contains declarations that won't match the predicate

void some_function(void);

struct SomeStruct {
    int x;
    int y;
};
