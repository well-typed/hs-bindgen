#ifndef ENUMS_H
#define ENUMS_H

#include <stdio.h>

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

enum packad {
    PACKED_A, PACKED_B, PACKED_C
} __attribute__((packed));

#endif /* ENUMS_H */
