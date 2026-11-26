#ifndef UNTAGGED_ENUM_FIXED_TYPE_H
#define UNTAGGED_ENUM_FIXED_TYPE_H

#include <stdint.h>

// C23 lets an enum fix its underlying type; Clang accepts this as an extension
// in earlier standards. The constants of an untagged enum become pattern
// synonyms of that type.

typedef unsigned char small;

// Underlying type is a typedef in this header
enum : small {
    SMALL_A,
    SMALL_B
};

// Underlying type is a typedef with an external binding
enum : uint8_t {
    BYTE_A,
    BYTE_B = 255
};

// Underlying type is a primitive type
enum : unsigned short {
    SHORT_A
};

#endif /* UNTAGGED_ENUM_FIXED_TYPE_H */
