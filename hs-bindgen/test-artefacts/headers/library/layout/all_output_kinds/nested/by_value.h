#ifndef ALL_OUTPUT_KINDS_NESTED_BY_VALUE_H
#define ALL_OUTPUT_KINDS_NESTED_BY_VALUE_H

// A header in a subdirectory gets the module AllOutputKinds.Nested.By_value.

#define BY_VALUE_FIELDS 2

struct by_value {
  float x;
  float y;
};

// Functions give the Safe, Unsafe and FunPtr modules.
float by_value_sum(struct by_value v);
struct by_value by_value_add(struct by_value a, struct by_value b);

// A global gives the Global module.
extern const struct by_value by_value_zero;

#endif
