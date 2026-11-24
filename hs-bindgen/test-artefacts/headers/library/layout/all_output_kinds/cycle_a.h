#ifndef ALL_OUTPUT_KINDS_CYCLE_A_H
#define ALL_OUTPUT_KINDS_CYCLE_A_H

#include "nested/by_value.h"

// struct cycle_a and struct cycle_b point at each other, so the types modules
// of cycle_a.h and cycle_b.h import each other with SOURCE imports.

#define CYCLE_A_LIMIT 64

struct cycle_b;

struct cycle_a {
  struct by_value position;
  struct cycle_b *first;
  int count;
};

struct cycle_a *cycle_a_new(struct by_value position);

// The raw function pointer type mentions library types, so its ToFunPtr and
// FromFunPtr instances go to the global Callback module.
void cycle_a_on_pair(struct cycle_a *a, void (*raw_callback)(struct cycle_b *x, struct cycle_b *y));

extern int cycle_a_total;

#endif
