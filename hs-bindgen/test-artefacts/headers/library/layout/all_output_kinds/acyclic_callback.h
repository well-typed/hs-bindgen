#ifndef ALL_OUTPUT_KINDS_ACYCLIC_CALLBACK_H
#define ALL_OUTPUT_KINDS_ACYCLIC_CALLBACK_H

// A typedef callback of a header outside every import cycle: its instances
// stay in the types module.

enum callback_code { CALLBACK_CODE_A, CALLBACK_CODE_B };

typedef void (*acyclic_callback)(enum callback_code code, int value);

void acyclic_callback_register(acyclic_callback callback);

#endif
