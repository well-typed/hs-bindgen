#ifndef BOOT_FILES_B_H
#define BOOT_FILES_B_H

#include "a.h"

// A macro value and a function-like macro that use the macros of the other
// cycle member, through its boot file.
#define B_VALUE (A_VALUE / 2)
#define B_FUNCTION(n) (A_FUNCTION(n) + 1)

// The FFIType of this typedef reduces through the enum of the other cycle
// member.
typedef enum a_enum peer_enum;

struct b {
  struct a *a;
  peer_enum e;
};

peer_enum b_by_value(peer_enum e);

#endif
