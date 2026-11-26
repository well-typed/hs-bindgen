#ifndef MODULE_NAMES_SAME_BASENAME_H
#define MODULE_NAMES_SAME_BASENAME_H

#define SAME_BASENAME_MAX 4096

// The typedef and the struct it points to share a name. They are declared in
// different headers, so they get the Haskell name Same_basename in two
// modules, and neither gets a suffix.
typedef struct same_basename *same_basename;

same_basename same_basename_open(int width);
void same_basename_close(same_basename s);

#endif
