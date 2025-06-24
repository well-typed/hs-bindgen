/* This example header contains various definitions
 * to smoke test hs-bindgen code generation end-to-end.
 * See test-th and test-pp tests.
 */

#ifndef TEST_01_H
#define TEST_01_H

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

static inline struct StructFLAM *flam_alloc(int n) {
    struct StructFLAM *ptr = malloc(sizeof(struct StructFLAM) + sizeof(long) * n);
    if (ptr) {
        ptr->length = n;
        for (int i = 0; i < n; ++i) {
            ptr->numbers[i] = i; // maybe do something more interesting?
        }
    }
    return ptr;
}

static inline void flam_free(struct StructFLAM *ptr) {
    free(ptr);
}

/* unions */

union longDouble {
    long long l;
    double d;
};

/* Basic enumeration. */
enum EnumBasic {
    ENUM_BASIC_A,
    ENUM_BASIC_B,
    ENUM_BASIC_C
};

/* Enumeration starting with negative value. */
enum EnumNeg {
    ENUM_NEG_A = -1,
    ENUM_NEG_B,
    ENUM_NEG_C
};

/* Enumeration with non-sequential values. */
enum EnumNonSeq {
    ENUM_NON_SEQ_A = 200,
    ENUM_NON_SEQ_B = 301,
    ENUM_NON_SEQ_C = 404
};

/* Enumeration with multiple constants with the same value. */
enum EnumSame {
    ENUM_SAME_A = 0,
    ENUM_SAME_B = 100,
    ENUM_SAME_C = 100,
    ENUM_SAME_D = 200
};

/* Struct arguments and result types */

struct thing {
    int x;
};

static inline int thing_fun_1(struct thing x) {
    return x.x;
}

static inline struct thing thing_fun_2(int x) {
    struct thing res = { .x = x };
    return res;
}

static inline struct thing thing_fun_3(struct thing x) {
    struct thing res = { .x = x.x * 2 };
    return res;
}

#endif
