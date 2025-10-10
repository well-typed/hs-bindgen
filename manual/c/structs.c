#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "structs.h"

struct surname *surname_alloc(const char nm[]) {
  size_t n = strlen(nm);
  struct surname *ptr = malloc(sizeof(struct surname) + n * sizeof(char));
  if (ptr) {
    ptr->len = n;
    memcpy(ptr->data, nm, n);
  }
  return ptr;
}

void surname_free(struct surname *ptr) { free(ptr); }

// Unimplemented.
struct square *create_square(double side_length) { return NULL; };
