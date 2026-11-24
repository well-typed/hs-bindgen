#ifndef SQUASHED_ACROSS_HEADERS_FWD_H
#define SQUASHED_ACROSS_HEADERS_FWD_H

// A forward declaration header. Both typedefs are squashed into the structs
// of a.h and b.h, so nothing is left for this header and it writes no module.
typedef struct a a;
typedef struct b b;

#endif
