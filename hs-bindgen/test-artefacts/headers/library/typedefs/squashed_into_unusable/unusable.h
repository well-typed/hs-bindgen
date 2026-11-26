#ifndef SQUASHED_INTO_UNUSABLE_UNUSABLE_H
#define SQUASHED_INTO_UNUSABLE_UNUSABLE_H

// struct ld fails to parse, because long double is unsupported, so the library
// plan never places it. Name mangling still squashes ld_t into it.
struct ld {
  long double x;
};

typedef struct ld ld_t;

struct kept {
  int x;
};

#endif
