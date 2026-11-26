#ifndef ALL_OUTPUT_KINDS_CYCLE_B_H
#define ALL_OUTPUT_KINDS_CYCLE_B_H

#include "nested/by_value.h"
#include "cycle_a.h"

// The macro uses a macro of the other cycle member, through its boot file.
#define CYCLE_B_LIMIT (CYCLE_A_LIMIT * 2)

// A typedef callback of a header in an import cycle: its instances go to the
// per-header Callback module in library mode when in a cycle.
typedef void (*cycle_b_callback)(struct cycle_b *self, float dt);

struct cycle_b {
  struct cycle_a *owner;
  struct cycle_b *next;
  struct by_value position;
  cycle_b_callback callback;
};

struct cycle_b *cycle_b_new(struct cycle_a *owner, struct by_value position);

#endif
