/* This example header contains various definitions
 * to smoke test hs-bindgen code generation end-to-end.
 * See test-th and test-pp tests.
 */

#ifndef TEST01_H
#define TEST01_H

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

/* flexible array member */

struct StructFLAM {
    int length;
    long numbers[];
};

static inline struct StructFLAM *flam_init(int n) {
    struct StructFLAM *ptr = malloc(sizeof(struct StructFLAM) + sizeof(long) * n);
    if (ptr) {
        ptr->length = n;
        for (int i = 0; i < n; ++i) {
            ptr->numbers[i] = i; // maybe do something more interesting?
        }
    }
    return ptr;
}

static inline void flam_deinit(struct StructFLAM *ptr) {
    free(ptr);
}

#endif
