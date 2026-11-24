#ifndef SQUASHED_ACROSS_HEADERS_PRIVATE_HANDLE_H
#define SQUASHED_ACROSS_HEADERS_PRIVATE_HANDLE_H

#include "../handle.h"

struct handle {
  a *owner;
  int uses;
};

#endif
