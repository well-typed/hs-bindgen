/* This example header contains various definitions
 * to smoke test hs-bindgen code generation end-to-end.
 * See test-th and test-pp tests.
 */

/* Standard headers. */
#include <stdlib.h>

/* Basic structure. Nothing special. */
struct StructBasic {
    int field1;
    char field2;
};

/* Struct with fixed size array. */
struct StructFixedSizeArray {
    int x;
    char ys[5];
};

/* A function like macro. */
#define PLUS(x,y) x + y + 1L

/* Structure with bitfield definions. */
struct StructBitfield {
    int a;
    unsigned int b : 1;
    unsigned int c : 1;
    int d : 5;
};

/* Function declaration. */
static inline int my_fma(int x, int y, int z) {
    return x * y + z;
}
