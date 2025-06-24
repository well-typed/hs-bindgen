#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "structs.h"

struct surname *surname_init(char nm[]) {
  size_t n = strlen(nm) + 1;
  struct surname *ptr = malloc(sizeof(struct surname) + n * sizeof(char));
  if (ptr) {
    ptr->len = n;
    memcpy(ptr->data, nm, n);
  }
  return ptr;
}

void surname_deinit(struct surname *ptr) { free(ptr); }
