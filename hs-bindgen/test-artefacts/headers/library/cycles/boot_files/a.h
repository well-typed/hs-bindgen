#ifndef BOOT_FILES_A_H
#define BOOT_FILES_A_H

// struct a and struct b point at each other: the types modules of a.h and
// b.h form an import cycle, so each gets a boot file.

#define A_VALUE 1024
#define A_FUNCTION(n) ((n) * 2)

enum a_enum { A_ONE, A_TWO };

struct b;

struct a {
  struct b *b;
  enum a_enum e;
};

struct a *a_new(enum a_enum e);

#endif
