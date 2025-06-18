#ifndef ENUMS_H
#define ENUMS_H

enum first {
    FIRST1,
    FIRST2
};

enum second {
    SECOND_A = -1,
    SECOND_B,
    SECOND_C
};

enum same {
    SAME_A = 1,
    SAME_B = 1,
};

enum nonseq {
    NONSEQ_A = 200,
    NONSEQ_B = 301,
    NONSEQ_C = 404
};

enum packed {
    PACKED_A, PACKED_B, PACKED_C
} __attribute__((packed));

typedef enum { A_FOO, A_BAR } enumA;

typedef enum enumB { B_FOO, B_BAR } enumB;

enum enumC { C_FOO, C_BAR };
typedef enum enumC enumC;

enum enumD { D_FOO, D_BAR };
typedef enum enumD enumD_t;

#endif /* ENUMS_H */
