/* A header-only library.
 *
 * The declarations are always visible. The definitions are compiled only into
 * the one translation unit that defines HEADER_ONLY_IMPLEMENTATION -- here,
 * hs/manual/cbits/header_only.c. Defining it in more than one translation unit
 * results in duplicate symbols at link time.
 */

#ifndef HEADER_ONLY_H
#define HEADER_ONLY_H

/**
 * The number of horns of a unicorn.
 */
int header_only_horns(void);

#ifdef HEADER_ONLY_IMPLEMENTATION

int header_only_horns(void) { return 1; }

#endif

#endif
