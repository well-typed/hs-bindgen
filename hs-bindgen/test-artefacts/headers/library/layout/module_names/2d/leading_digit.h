#ifndef MODULE_NAMES_2D_LEADING_DIGIT_H
#define MODULE_NAMES_2D_LEADING_DIGIT_H

#include "../same_basename.h"
#include "../dash-name.h"

// The directory 2d starts with a digit, so its module name component gets a
// C prefix.

// A macro that uses another header's macro, through a normal import.
#define LEADING_DIGIT_MAX (SAME_BASENAME_MAX / 2)

void leading_digit_fill(same_basename s, struct dash_name value);

extern int leading_digit_count;

#endif
